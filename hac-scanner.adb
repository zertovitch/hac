with HAC.Data;    use HAC.Data;
with HAC.UErrors; use HAC.UErrors;

with Ada.Text_IO; use Ada.Text_IO;

package body HAC.Scanner is

  package IIO is new Integer_IO (Integer);
  use IIO;

  procedure InSymbol is
    I, J, K, e : Integer;
    theLine    : String (1 .. 1000);

    function UpCase (c : Character) return Character is
    begin
      if 'a' <= c and then c <= 'z' then
        return Character'Val
                (Character'Pos (c) -
                 Character'Pos ('a') +
                 Character'Pos ('A'));
      else
        return c;
      end if;
    end UpCase;

    procedure NextCh is -- Read Next Char; process line end
    begin
      if CC = LL then
        if cEndOfSource then
          if qDebug then  -- @@@ Make an error for this
            Put_Line (" PROGRAM INCOMPLETE");
          end if;
          if ListingWasRequested then
            Put_Line (Listing, " PROGRAM INCOMPLETE");
          end if;
          Error (err_program_incomplete);
          ErrorMsg;
          raise Failure_1_0;
        end if;
        if SkipFlag then
          EndSkip;
        end if;
        if ListingWasRequested then
          New_Line (Listing);
        end if;
        LineCount := LineCount + 1;
        if ListingWasRequested then
          Put (Listing, LineCount, 4);
          Put (Listing, "  ");
          Put (Listing, LC, 5);
          Put (Listing, "  ");
        end if;
        LL := 0;
        CC := 0;
        cGetNextLine (theLine, LL); -- Ada style
        InpLine (1 .. LL + 1) := theLine (1 .. LL) & ' '; -- Should be
                                                          --truncated to LLNG
        syLine                := LineCount;
        LL                    := LL + 1;

        if ListingWasRequested then
          New_Line (Listing);
          Put_Line (Listing, InpLine);
        end if;
      end if;

      CC := CC + 1;
      CH := InpLine (CC);
      -- Manuel : Change tabs for spaces
      if Character'Pos (CH) = 9 then
        CH := ' '; -- IdTab for space
      end if;
      if Character'Pos (CH) < Character'Pos (' ') then
        Error (err_control_character);
      end if;

    end NextCh;

    procedure ReadScale is
      S, Sign : Integer;
    begin
      NextCh;
      Sign := 1;
      S    := 0;
      if CH = '+' then
        NextCh;
      elsif CH = '-' then
        NextCh;
        Sign := -1;
      end if;
      if CH not in '0' .. '9' then
        Error (err_illegal_parameters_to_Get);
      else
        loop
          S := 10 * S + Character'Pos (CH) - Character'Pos ('0');
          NextCh;
          exit when CH not in '0' .. '9';
        end loop;
      end if;
      e := S * Sign + e;
    end ReadScale;

    procedure AdjustScale is
      S    : Integer;
      D, T : Float;
    begin
      if K + e > EMax then
        Error (err_number_too_large);
      elsif K + e < EMin then
        RNum := 0.0;
      else
        S := abs e;
        T := 1.0;
        D := 10.0;
        loop
          while S rem 2 = 0 loop -- NOT Odd(S)
            S := S / 2;
            D := D ** 2;
          end loop;

          S := S - 1;
          T := D * T;
          exit when S = 0;
        end loop;

        if e >= 0 then
          RNum := RNum * T;
        else
          RNum := RNum / T;
        end if;
      end if;
    end AdjustScale;

  begin -- InSymbol

    <<Label_1>>
    while CH = ' ' loop
      NextCh;
    end loop;

    syStart := CC - 1;
    if CharacterTypes (CH) = Illegal then
      Error (err_illegal_character);
      if qDebug then
        Put_Line (" Char is => " & Integer'Image (Character'Pos (CH)));
      end if;
      if ListingWasRequested then
        Put_Line
         (Listing,
          " Char is => " & Integer'Image (Character'Pos (CH)));
      end if;
      NextCh;
      goto Label_1;

    end if;

    case CH is
    when 'A' .. 'Z' |  -- identifier or wordsymbol
         'a' .. 'z' =>
      K  := 0;
      Id := (others => ' ');
      loop
        if K < Alng then
          K      := K + 1;
          Id (K) := UpCase (CH);
        else
          Error(err_identifier_too_long, Id);
        end if;
        NextCh;
        exit when CH /= '_'
                 and then special_or_illegal (CharacterTypes (CH));
      end loop;

      I := 1;
      J := AdaKeyW'Last; -- Binary Search
      loop
        K := (I + J) / 2;
        if Id(AdaKeyW_String'Range) <= AdaKeyW (K).st then
          J := K - 1;
        end if;
        if Id(AdaKeyW_String'Range) >= AdaKeyW (K).st then
          I := K + 1;
        end if;
        exit when I > J;
      end loop;

      if I - 1 > J then
        Sy := AdaKeyW (K).sy;
      else
        Sy := IDent;
      end if;
      if Sy = USy then
        Sy := IDent;
        Error (err_Ada_reserved_word);
      end if;

    when '0' .. '9' =>  -- Number
      K    := 0;
      INum := 0;
      Sy   := IntCon;
      loop

        INum := INum * 10 + Character'Pos (CH) - Character'Pos ('0');
        K    := K + 1;
        NextCh;
        exit when CharacterTypes (CH) /= Number;
      end loop;

      if K > KMax then
        Error (err_number_too_large);
        INum := 0;
        K    := 0;
      end if;
      if CH = '.' then
        NextCh;
        if CH = '.' then
          CH := Character'Val (128);
        else
          Sy   := FloatCon;
          RNum := Float (INum);
          e    := 0;
          while CharacterTypes (CH) = Number loop
            e    := e - 1;
            RNum := 10.0 * RNum +
                    Float (Character'Pos (CH) - Character'Pos ('0'));
            NextCh;
          end loop;

          if e = 0 then
            Error (err_illegal_parameters_to_Get);
          end if;
          if CH = 'E' then
            ReadScale;
          end if;
          if e /= 0 then
            AdjustScale;
          end if;
        end if;

      elsif CH = 'E' then

        Sy   := FloatCon;
        RNum := Float (INum);
        e    := 0;
        ReadScale;
        if e /= 0 then
          AdjustScale;
        end if;

      end if;

    when ':' =>
      NextCh;
      if CH = '=' then
        Sy := Becomes;
        NextCh;
      else
        Sy := Colon;
      end if;

    when '<' =>
      NextCh;
      if CH = '=' then
        Sy := LEQ;
        NextCh;
      else
        Sy := LSS;
      end if;

    when '>' =>
      NextCh;
      if CH = '=' then
        Sy := GEQ;
        NextCh;
      else
        Sy := GTR;
      end if;

    when '/' =>
      NextCh;
      if CH = '=' then
        Sy := NEQ;
        NextCh;
      else
        Sy := Divide;
      end if;

    when '.' =>
      NextCh;
      if CH = '.' then
        Sy := RANGE_Symbol; -- !! Huh ?!
        NextCh;
      else
        Sy := Period;
      end if;

    when c128 =>  -- Hathorn
      Sy := RANGE_Symbol;  -- !! Huh ?!
      NextCh;

    when '"' =>
      K := 0;
      <<Label_2>> NextCh;
      if CH = '"' then
        NextCh;
        if CH /= '"' then
          goto Label_3;
        end if;
      end if;
      if Sx + K = SMax then
        Fatal (STRING_table_overflow);
      end if;
      StringTab (Sx + K) := CH;
      K                  := K + 1;
      if CC = 1 then
        K := 0; -- END OF InpLine
      else
        goto Label_2;
      end if;
      <<Label_3>>
      Sy    := StrCon;
      INum  := Sx;
      SLeng := K;
      Sx    := Sx + K;

    when ''' => -- Character literal (code reused from Pascal string literal, hence a loop)
      K := 0;
      <<Label_4>> NextCh;
      if CH = ''' then
        NextCh;
        if CH /= ''' then
          goto Label_5;
        end if;
      end if;
      if Sx + K = SMax then
        Fatal (STRING_table_overflow);
      end if;
      StringTab (Sx + K) := CH;
      K                  := K + 1;
      if CH = ''' and K = 1 then -- '''
        NextCh;
        goto Label_5;
      end if;
      if CC = 1 then
        K := 0;   -- END OF InpLine
      else
        goto Label_4;
      end if;
      <<Label_5>>
      if K = 1 then
        Sy   := CharCon;
        INum := Character'Pos (StringTab (Sx));
      elsif K = 0 then
        Error (err_character_zero_chars);
        Sy   := CharCon;
        INum := 0;
      else
        Error (err_character_delimeter_used_for_string);
        Sy    := StrCon;
        INum  := Sx;
        SLeng := K;
        Sx    := Sx + K;
      end if;

    when '-' =>
      NextCh;
      if CH /= '-' then
        Sy := MinUS;
      else --  comment
        CC := LL; -- ignore rest of input line
        NextCh;
        goto Label_1;
      end if;

    when '=' =>
      NextCh;
      if CH /= '>' then
        Sy := EQL;
      else
        Sy := Finger;
        NextCh;
      end if;

    when '{' =>  -- Special non documented comment !! O_o: remove that !!

      while CH /= '}' loop
        NextCh;
      end loop;

      NextCh;
      goto Label_1;

    when '|' =>
      Sy := Alt;
      NextCh;

    when '+' | '*' | '(' | ')' | ',' | '[' | ']' | ';' | '&' =>
      Sy := SpecialSymbols (CH);
      NextCh;

    when '$' | '!' | '@' | '\' | '^' | '_' | '?' | '%' =>
      --  duplicate case Constant '&',
      Error (err_illegal_character);
      if qDebug then
        Put_Line (" [ $!@\^_?""&%  ]");
      end if;
      if ListingWasRequested then
        Put_Line (Listing, " [ $!@\^_?""&%  ]");
      end if;
      NextCh;
      goto Label_1;

    when Character'Val (0) .. ' ' =>
      null;
    when others =>
      null;

    end case; -- CH

    syEnd := CC - 1;

    if qDebug then
      Put_Line(Sym_dump,InpLine(1..LL));
      for i in 1..CC-2 loop
        Put(Sym_dump,'.');
      end loop;
      Put_Line(Sym_dump,"^");
      Put (Sym_dump,
        '[' & Integer'Image(LineCount) & ':' & Integer'Image(CC) & ":] " &
        KeyWSymbol'Image (Sy)
      );
      case Sy is
        when IDent =>
          Put (Sym_dump, ": " & Id);
        when IntCon =>
          Put (Sym_dump, ": " & Integer'Image (INum));
        when FloatCon =>
          Put (Sym_dump, ": " & Float'Image (RNum));
        when StrCon =>
          Put (Sym_dump, ": """);
          for i in INum .. INum + SLeng - 1 loop
            Put (Sym_dump, StringTab (i));
          end loop;
          Put (Sym_dump, '"');
        when Becomes =>
          Put (Sym_dump, " := ");
        when Colon =>
          Put (Sym_dump, " : ");
        when CONSTANT_Symbol =>
          Put (Sym_dump, " constant ");
        when others =>
          null;
      end case;
      New_Line(Sym_dump, 2);
    end if;

  end InSymbol;

end HAC.Scanner;
