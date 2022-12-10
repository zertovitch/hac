--  https://www.mathekalender.de/wp/de/kalender/aufgaben/2022-04-de/
--
--  Brute-Force-Methode um Raestsels "Geschenkeversand"
--  Moeglichkeit Nr 4 ("Fuer jede Anfangsreihe von Geschenken mit einer
--  ungeraden Anzahl von ordentlich verpackten Geschenken") zu testen.
--  Das Programm ist kein Beweis, dass es immer geht, aber man sieht,
--  dass dieselbe Muster immer wieder kommen, von '-' getrennt.
--
--    P = ordentlich verpackt
--    D = Verpackungsmaterial beschaedigt
--
--  Fuer die anderen Moeglichkeiten gibt es Gegenbeispiele:
--    PPPP hat keine Loesung aber gilt fuer Nr 1, 3, 7, 8, 9.
--    PDP hat keine Loesung aber gilt mindestens fuer Nr 2, 5, 6.
--    PPDDD hat keine Loesung aber gilt mindestens fuer Nr 10.

with HAT;

procedure Mathe_Plus_2022_04 is

  verbose : constant Boolean := True;

  subtype Test_String is String (1 .. 10);

  s : Test_String;

  successes, failures : Natural := 0;

  use HAT;

  procedure Solve is
    run : Test_String := s;
    --
    procedure Flip (i : Integer) is
    begin
      if i in run'Range then
        case run (i) is
          when 'P' => run (i) := 'D';
          when 'D' => run (i) := 'P';
          when others => null;
        end case;
      end if;
    end Flip;
    --
    Ps, Ds, done : Boolean;
  begin
    if verbose then
      Put_Line (+run & " <------");
    end if;
    for count in 1 .. run'Length * 2 loop
      Ps := False;
      Ds := False;
      for i in run'Range loop
        case run (i) is
          when 'P' =>
            --  Simple choice: just choose the first 'P' on the line.
            Ps := True;
            Flip (i - 1);
            Flip (i + 1);
            run (i) := '-';  --  Send gift number i.
            exit;
          when 'D' =>
            Ds := True;
          when others =>
            null;
        end case;
      end loop;
      if verbose then
        Put_Line (run);
      end if;
      done := not (Ps or Ds);
      --  ^ done = True: no more gifts in either state: line is empty.
      exit when done;
    end loop;
    if done then
      successes := successes + 1;
    else
      failures := failures + 1;
    end if;
  end Solve;

  procedure Test
    (start : Positive; number_of_Ps : Natural; length : Positive) is
  begin
    if number_of_Ps = 0 then
      for i in start .. length loop
        s (i) := 'D';
      end loop;
      for i in length + 1 .. s'Last loop
        s (i) := ' ';
      end loop;
      Solve;
    else
      if start + number_of_Ps - 1 < length then
        s (start) := 'D';
        Test (start + 1, number_of_Ps, length);
      end if;
      s (start) := 'P';
      Test (start + 1, number_of_Ps - 1, length);
    end if;
  end;
begin
  --  Test all setups up to a certain
  --  length and with an odd number of 'P'.
  for length in 1 .. s'Length loop
    for np in 1 .. length loop
      if np mod 2 = 1 then
        Test (1, np, length);
      end if;
    end loop;
  end loop;
  Put_Line (+"successes . . " & successes);
  Put_Line (+"failures  . . " & failures);
end Mathe_Plus_2022_04;
