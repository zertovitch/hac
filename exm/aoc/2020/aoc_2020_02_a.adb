--  Solution to Advent of Code 2020, Day 2, part 1.
---------------------------------------------------
--  Each line gives the password policy and then the password.
--  The password policy indicates the lowest and highest number of times
--  a given letter must appear for the password to be valid.
--  For example, 1-3 a means that the password must contain 'a'
--  at least 1 time and at most 3 times.
--
--  1-3 a: abcde     -> valid (1 time 'a')
--  1-3 b: cdefg     -> invalid (0 times 'b')
--  2-9 c: ccccccccc -> valid (9 times 'c')
--
--  https://adventofcode.com/2020/day/2
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_02_a is
  s : VString;
  f : File_Type;
  c, sep : Character;
  i, n, valid, min, max : Integer;
begin
  Open (f, "aoc_2020_02.txt");
  valid := 0;
  while not End_Of_File (f) loop
    Get (f, min);
    Get (f, sep);
    Get (f, max);
    Get (f, sep);
    Get (f, c);
    Get (f, sep);
    Get (f, sep);
    Get_Line (f, s);
    i := 1;
    n := 0;
    while i <= Length (s) loop
      if c = Element (s, i) then n := n + 1; end if;
      i := i + 1;
    end loop;
    if (n >= min) and (n <= max) then
      valid := valid + 1;
    else
      Put_Line (+"Invalid: " & min & ',' & max &
                ',' & c & "  -->  " & n & "  |----|  " & s);
    end if;
  end loop;
  Put_Line (+"Valid passwords (a): " & valid);
  Close (f);
end AoC_2020_02_a;
