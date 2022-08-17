--  Solution to Advent of Code 2020, Day 2
------------------------------------------
--  Password Philosophy
--
--  https://adventofcode.com/2020/day/2
--
--  Part One
------------
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
--  Part Two
------------
--  Each policy actually describes two positions in the password, where 1
--  means the first character, 2 means the second character, and so on.
--  Exactly one of these two positions must contain the given letter.
--
--  1-3 a: abcde     is valid   : position 1 contains 'a' and position 3 does not.
--  1-3 b: cdefg     is invalid : neither position 1 nor position 3 contains 'b'.
--  2-9 c: ccccccccc is invalid : both position 2 and position 9 contain 'c'.
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_02 is
  s : VString;
  f : File_Type;
  c, sep1, sep2, sep3, sep4 : Character;
  i, n, valid, arg1, arg2 : Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;
begin
  for part in 1 .. 2 loop
    Open (f, "aoc_2020_02.txt");
    valid := 0;
    while not End_Of_File (f) loop
      Get (f, arg1);
      Get (f, sep1);
      Get (f, arg2);
      Get (f, sep2);
      Get (f, c);
      Get (f, sep3);
      Get (f, sep4);
      Get_Line (f, s);
      i := 1;
      n := 0;
      while i <= Length (s) loop
        if part = 1 then
          if c = Element (s, i) then n := n + 1; end if;
        else
          if c = Element (s, i) and (i = arg1 or i = arg2) then n := n + 1; end if;
        end if;
        i := i + 1;
      end loop;
      if (part = 1 and n >= arg1 and n <= arg2) or (part = 2 and n = 1) then
        valid := valid + 1;
      elsif verbose then
        Put_Line (+"Invalid: " & arg1 & ',' & arg2 &
                  ',' & c & "  -->  " & n & "  |----|  " & s);
      end if;
    end loop;
    Close (f);
    if compiler_test_mode then
      if valid /= Integer_Value (Argument (part)) then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
    else
      Put_Line (+"Valid passwords (part " & part & "): " & valid);
    end if;
  end loop;
  --  Part 1: validated by AoC: 607
  --  Part 2: validated by AoC: 321
end AoC_2020_02;
