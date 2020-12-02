--  Solution to Advent of Code 2020, Day 2, part 2.
---------------------------------------------------
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
  c : Character;
  i, j, n, valid, pos1, pos2 : Integer;
begin
  Open (f, "aoc_2020_02.txt");
  valid := 0;
  while not End_Of_File (f) loop
    Get_Line (f, s);
    pos1 := 0;
    pos2 := 0;
    i := 1;
    loop
      c := Element (s, i);
      exit when c = '-';
      pos1 := pos1 * 10 + (Ord (c) -  Ord ('0'));
      i := i + 1;
    end loop;
    i := i + 1;
    loop
      c := Element (s, i);
      exit when c = ' ';
      pos2 := pos2 * 10 + (Ord (c) -  Ord ('0'));
      i := i + 1;
    end loop;
    i := i + 1;
    c := Element (s, i);
    i := i + 3;
    n := 0;
    j := 1;
    loop
      if (c = Element (s, i)) and ((j = pos1) or (j = pos2)) then n := n + 1; end if;
      exit when i = Length (s);
      i := i + 1;
      j := j + 1;
    end loop;
    if n = 1 then
      valid := valid + 1;
    else
      Put_Line (+"Invalid: " & pos1 & ',' & pos2 &
                ',' & c & "  -->  " & n & "  |----|  " & s);
    end if;
  end loop;
  Put_Line (+"Valid passwords (b): " & valid);
  Close (f);
end AoC_2020_02_b;
