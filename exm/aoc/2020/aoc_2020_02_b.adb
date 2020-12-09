--  Solution to Advent of Code 2020, Day 2, part 2.
---------------------------------------------------
--  Password Philosophy
--
--  Each policy actually describes two positions in the password, where 1
--  means the first character, 2 means the second character, and so on.
--  Exactly one of these two positions must contain the given letter.
--
--  1-3 a: abcde     is valid   : position 1 contains 'a' and position 3 does not.
--  1-3 b: cdefg     is invalid : neither position 1 nor position 3 contains 'b'.
--  2-9 c: ccccccccc is invalid : both position 2 and position 9 contain 'c'.
--
--  https://adventofcode.com/2020/day/2
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_02_b is
  s : VString;
  f : File_Type;
  c, sep1, sep2, sep3, sep4 : Character;
  i, n, valid, pos1, pos2 : Integer;
  test_mode : constant Boolean := Argument_Count > 0;
begin
  Open (f, "aoc_2020_02.txt");
  valid := 0;
  while not End_Of_File (f) loop
    Get (f, pos1);
    Get (f, sep1);
    Get (f, pos2);
    Get (f, sep2);
    Get (f, c);
    Get (f, sep3);
    Get (f, sep4);
    Get_Line (f, s);
    i := 1;
    n := 0;
    while i <= Length (s) loop
      if (c = Element (s, i)) and ((i = pos1) or (i = pos2)) then n := n + 1; end if;
      i := i + 1;
    end loop;
    if n = 1 then
      valid := valid + 1;
    elsif not test_mode then
      Put_Line (+"Invalid: " & pos1 & ',' & pos2 &
                ',' & c & "  -->  " & n & "  |----|  " & s);
    end if;
  end loop;
  Close (f);
  if test_mode then
    if valid /= Integer_Value (Argument (1)) then
      Put ("*** Test FAILS ***");
    end if;
  else
    Put_Line (+"Valid passwords (b): " & valid);
  end if;
end AoC_2020_02_b;
