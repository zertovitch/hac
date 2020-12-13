--  Solution to Advent of Code 2020, Day 13
-------------------------------------------
--  Shuttle Search
--
--  https://adventofcode.com/2020/day/13
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_13 is

  --  Taken from MathPaqs (Euclidean_Ring_Tools, de-generic-ized for HAC):

  procedure GCD_and_Bezout (a, b : in Integer; s, t, the_gcd : out Integer) is
    --  Finds the GCD and s, t for the
    --  ` GCD (a, b) = a * s + b * t ` factorization (Bezout theorem).
    --  Program 1.8, Introduction to number theory, RBJT Allenby & EJ Redfern
    ta, tb : array (1 .. 3) of Integer;
    q, r : Integer;
  begin
    ta (1) := 1;         tb (1) := 0;
    ta (2) := 0;         tb (2) := 1;
    ta (3) := a;         tb (3) := b;
    while tb (3) /= 0 loop
      q := ta (3) / tb (3);
      for i in 1 .. 3 loop
        r := ta (i) - q * tb (i);
        ta (i) := tb (i);
        tb (i) := r;
      end loop;
    end loop;
    s :=       ta (1);
    t :=       ta (2);
    the_gcd := ta (3);
  end GCD_and_Bezout;

  answer_1, answer_2, d, dmin, earliest, idmin, prod, sk, tk, sum, gcd, n : Integer;
  f : File_Type;
  freq : array (1 .. 1000) of Integer;
  sep : Character;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;
 begin
  Open (f, "aoc_2020_13.txt");
  Get (f, earliest);
  n := 0;
  loop
    n:= n + 1;
    Get (f, freq (n));
    exit when End_of_File (f);
    Get (f, sep);
  end loop;
  Close (f);
  prod := 1;
  for k in 1 .. n loop
    if freq (k) > 0 then
      d := freq (k) * (1 + earliest / freq (k));  --  Next departure after earliest
      if k = 1 then
        dmin := d;
        idmin := 1;
      elsif d < dmin then
        dmin := d;
        idmin := freq (k);
      end if;
      prod := prod * freq (k);
      if verbose then
        Put_Line (
          +"  frequency: " & freq (k) &
          ", first departure: " & d &
          ", delta time: " & (k - 1)
        );
      end if;
    end if;
  end loop;
  if verbose then
    Put_Line (+"First bus for you : ID " & idmin);
  end if;
  answer_1 := (dmin - earliest) * idmin;
  --  Chinese remainder theorem (the frequencies are
  sum := 0;
  for k in 2 .. n loop
    if freq (k) > 0 then
      GCD_and_Bezout (prod / freq (k), freq (k), sk, tk, gcd);
      if gcd > 1 then
        Put_Line ("Not coprime, Chinese remainder theorem cannot be used.");
        --  return;
      end if;
      sum := sum + (freq (k) - k + 1) * sk * (prod / freq (k));
    end if;
  end loop;
  sum := sum mod prod;
  answer_2 := sum;
  --
  if compiler_test_mode then
    if (answer_1 /= Integer_Value (Argument (1))) or
       (answer_2 /= Integer_Value (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: first bus for you (encoded answer): " & answer_1);
    Put_Line (+"Part 2: earliest timestamp for first bus in correct sequence: " & answer_2);
  end if;
end AoC_2020_13;
