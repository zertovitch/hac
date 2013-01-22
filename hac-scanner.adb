with HAC.Data;                          use HAC.Data;
with HAC.UErrors;                       use HAC.UErrors;

with Ada.Text_IO;                       use Ada.Text_IO;

PACKAGE BODY HAC.Scanner IS

package IIO is new Integer_IO(integer); use IIO;

  PROCEDURE InSymbol IS
    I, J, K, e: Integer;
    theLine : String(1..1000);

    function UpCase(c: character) return character is
    begin
      if ('a' <= c) and then (c <= 'z') then
        return character'val(character'pos(c) - character'pos('a')
          + character'pos('A'));
      else
        return c;
      end if;
    end UpCase;

    PROCEDURE NextCh IS -- Read Next Char; process line end
   BEGIN
     IF CC = LL THEN
	IF cEndOfSource THEN
	  IF qDebug THEN		-- @@@ Make an error for this
	    Put_Line(" PROGRAM INCOMPLETE");
          END IF;
	  IF ListingWasRequested THEN
	    Put_Line(Listing," PROGRAM INCOMPLETE");
          END IF;
	  Error(81);
	  ErrorMsg;
	  raise Failure_1_0;
	END IF;
	IF SkipFlag THEN EndSkip; END IF;
	IF ListingWasRequested THEN New_Line(Listing); END IF;
	LineCount := LineCount + 1;
	IF ListingWasRequested THEN
          Put(Listing, LineCount, 4);
          Put(Listing, "  ");
          Put(Listing, LC, 5);
          Put(Listing, "  ");
	END IF;
	LL := 0;
	CC := 0;
	cGetNextLine(theLine, LL); -- Ada style
	InpLine(1..LL+1) := theLine(1..LL) & ' '; -- Should be truncated to LLNG
	syLine := LineCount;
	LL:= LL + 1;

	IF ListingWasRequested THEN
	  New_Line(Listing);
	  Put_Line(Listing, InpLine);
	END IF;
     END IF;

     CC := CC + 1;
     CH := InpLine(CC);
     -- Manuel : Change tabs for spaces
     IF  Character'Pos(CH) = 9 THEN
       CH := ' ';	-- IdTab for space
     END IF;
     IF  Character'Pos(CH) < Character'Pos(' ') THEN
	Error(60);
     END IF;

    END NextCh;

    PROCEDURE ReadScale IS
      S, Sign: Integer;
      BEGIN
	NextCh;
	Sign := 1;
	S := 0;
	IF  CH = '+' THEN
		NextCh;
	ELSIF  CH = '-' THEN
		NextCh;
		Sign := - 1;
	END IF;
	IF CH NOT IN '0'..'9' THEN
	  Error(40);
	ELSE
	  LOOP
	    S := 10 * S + Character'Pos(CH) - Character'Pos('0');
	    NextCh;
	    EXIT WHEN CH NOT IN '0'..'9';
	  END LOOP;
	END IF;
	e := S * Sign + e;
      END ReadScale;

    PROCEDURE AdjustScale IS
		S: Integer;
		D, T: Float;
	BEGIN
		IF  K + e > EMax THEN
			Error(21);
		ELSIF  K + e < EMin THEN
			RNum := 0.0;
		ELSE
			S := abs(e);
			T := 1.0;
			D := 10.0;
			LOOP
			  WHILE  S rem 2 = 0  LOOP -- NOT Odd(S)
					S := S  /  2;
					D := D ** 2;
			  END LOOP;

			  S := S - 1;
			  T := D * T;
			  EXIT WHEN  S = 0;
			END LOOP;

			IF  e >= 0 THEN
				RNum := RNum * T;
			ELSE
				RNum := RNum / T;
			END IF;
		END IF;
	END AdjustScale;

BEGIN	--{ InSymbol }

<<Label_1>>
	WHILE CH = ' ' LOOP NextCh; END LOOP;

	syStart := CC - 1;
	IF  CharacterTypes(CH) = Illegal THEN
		Error(24);
		IF  qDebug THEN
			Put_Line(" Char is => " &
			 integer'image(Character'Pos(CH)) );
		END IF;
		IF  ListingWasRequested THEN
			Put_Line(Listing, " Char is => " &
			 integer'image(Character'Pos(CH)) );
		END IF;
		NextCh;
		GOTO Label_1;

	END IF;

	CASE  CH  IS
          WHEN 'A'..'Z' |  -- identifier or wordsymbol
               'a'..'z'=>
	    		K := 0;
			Id := (others => ' ');
			LOOP
			  IF  K < Alng THEN
			    K := K + 1;
			    Id(K) := UpCase(CH);
			  END IF;
			  NextCh;
			  EXIT WHEN  CH /= '_'  AND THEN
			    special_or_illegal(CharacterTypes(CH));
			END LOOP;

			I := 1;
			J := NKW;	-- Binary Search
			LOOP
			  K := (I + J)  /  2;
			  IF  Id <= AdaKeyW(K) THEN J := K - 1; END IF;
			  IF  Id >= AdaKeyW(K) THEN I := K + 1; END IF;
			 EXIT WHEN  I > J;
			END LOOP;

			IF  I - 1 > J THEN
				Sy := AdaKeyWSy(K)
			;ELSE
				Sy := IDent;END IF;
			IF  (Sy = USy) THEN
				Sy := IDent;
				Error(67);
			END IF;

		WHEN '0'..'9'=>  -- Number
			K := 0;
			INUM := 0;
			Sy := IntCon;
			LOOP

				INUM := INUM * 10 + Character'Pos(CH) - Character'Pos('0');
				K := K + 1;
				NextCh;
			 EXIT WHEN  CharacterTypes(CH) /= Number;
			END LOOP;

			IF  (K > KMax)  OR  (INUM > NMax) THEN
				Error(21);
				INUM := 0;
				K := 0;
			END IF;
			IF  CH = '.' THEN
				NextCh;
				IF  CH = '.' THEN
					CH := Character'Val(128);
				ELSE
					Sy := FloatCon;
					RNum := float(INUM);
					e := 0;
					WHILE  CharacterTypes(CH) = Number  LOOP
						e := e - 1;
						RNum := 10.0 * RNum +
				float(Character'Pos(CH) - Character'Pos('0'));
						NextCh;
					END LOOP;

					IF  e = 0 THEN
						Error(40);END IF;
					IF  CH = 'E' THEN
						ReadScale;END IF;
					IF  e /= 0 THEN	AdjustScale; END IF;
				END IF;

			ELSIF  CH = 'E' THEN

				Sy := FloatCon;
				RNum := float(INUM);
				e := 0;
				ReadScale;
				IF  e /= 0 THEN	AdjustScale; END IF;

			END IF;

		WHEN ':' =>
			NextCh;
			IF  CH = '=' THEN
				Sy := BecomeS;
				NextCh;
			ELSE
				Sy := Colon;
			END IF;

		WHEN '<' =>
			NextCh;
			IF  CH = '=' THEN
				Sy := LEQ;
				NextCh;
			ELSE
				Sy := LSS;END IF;

		WHEN '>' =>
			NextCh;
			IF  CH = '=' THEN
				Sy := GEQ;
				NextCh;
			ELSE
				Sy := GTR;
			END IF;

		WHEN '/' =>
			NextCh;
			IF  CH = '=' THEN
				Sy := NEQ;
				NextCh;
			ELSE
				Sy := Divide;
			END IF;

		WHEN '.' =>
			NextCh;
			IF  CH = '.' THEN
				Sy := RangeSy;
				NextCh;
			ELSE
				Sy := Period;
			END IF;

		WHEN c128=>  -- Hathorn
			Sy := RangeSy;
			NextCh;

		WHEN '"'=>
			K := 0;
<<Label_2>>
			NextCh;
			IF  CH = '"' THEN
				NextCh;
				IF  CH /= '"' THEN
					GOTO Label_3;
				END IF;
			END IF;
			IF  Sx + K = SMax THEN
				Fatal(7);END IF;
			StringTab(Sx + K) := CH;
			K := K + 1;
			IF  CC = 1 THEN
				K := 0; -- END OF InpLine
			ELSE
				GOTO Label_2;
			END IF;
		<<Label_3>>
			IF  K = 0 THEN
				Error(38);
				Sy := StrCon;
				INUM := 0;
			ELSE
				Sy := StrCon;
				INUM := Sx;
				SLeng := K;
				Sx := Sx + K;
			END IF;

		WHEN ''' =>
			K := 0;
<<Label_4>>
			NextCh;
			IF  CH = ''' THEN
				NextCh;
				IF  CH /= ''' THEN
					GOTO Label_5;
				END IF;
			END IF;
			IF  Sx + K = SMax THEN
				Fatal(7);END IF;
			StringTab(Sx + K) := CH;
			K := K + 1;
			IF  CC = 1 THEN
				K := 0;   -- END OF InpLine
			ELSE
				GOTO Label_4;
			END IF;
<<Label_5>>
			IF  K = 1 THEN
				Sy := CharCon;
				INUM := Character'Pos(StringTab(Sx));
			ELSIF  K = 0 THEN
				Error(38);
				Sy := CharCon;
				INUM := 0;
			ELSE
				Error(66);
				Sy := StrCon;
				INUM := Sx;
				SLeng := K;
				Sx := Sx + K;
			END IF;


		WHEN '-' =>
			NextCh;
			IF  CH /= '-' THEN
				Sy := MinUS;
			ELSE --  comment
				CC := LL; -- ignore rest of input line
				NextCh;
				GOTO Label_1;
			END IF;

		WHEN '=' =>
			NextCh;
			IF  CH /= '>' THEN
				Sy := EQL;
			ELSE
				Sy := Finger;
				NextCh;
			END IF;

		WHEN '{'=>  -- Special non documented comment

			WHILE  CH /= '}'  LOOP NextCh; END LOOP;

			NextCh;
			GOTO Label_1;

		WHEN '|' =>
			Sy := Alt;
			NextCh;

		WHEN '+'|'*' |  '('|')' |  ',' |  '['|']' |  ';' |  '&' =>
			Sy := SpecialSymbols(CH);
			NextCh;

		WHEN '$' |  '!' |  '@' |  '\' |  '^' |  '_' |  '?' |  '%'=>
			--  duplicate case Constant '&',
			Error(24);
			IF  qDebug THEN
				Put_Line(" [ $!@\^_?""&%  ]");
			END IF;
			IF  ListingWasRequested THEN
				Put_Line(Listing, " [ $!@\^_?""&%  ]");
			END IF;
			NextCh;
			GOTO Label_1;

	  when  character'val(0) .. ' ' => null;
	  when  others => null;

	END CASE; -- CH

	syEnd := CC - 1;

    if qDebug then
      Put("[Sym:] " & KeyWSymbol'Image(Sy));
      case Sy is
        when ident =>
          Put(": " & ID);
        when intcon =>
          Put(": " & Integer'Image(INUM));
        when becomes =>
          Put(" := ");
        when colon =>
          Put(" : ");
        when constsy =>
          Put(" constant ");
        when others =>
          null;
      end case;
      New_Line;
    end if;

  END InSymbol;


end HAC.Scanner;

