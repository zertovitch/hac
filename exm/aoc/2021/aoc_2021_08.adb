--  Solution to Advent of Code 2021, Day 08
-------------------------------------------
--  Seven Segment Search
--
--  https://adventofcode.com/2021/day/8
--  Copy of questions in: aoc_2021_08_questions.txt
--
with HAL;  --  For a build with "full Ada": files hal*.ad* are in ../../../src

procedure AoC_2021_08 is
  use HAL;
  --
  --  input : constant VString := +"mini.txt";
  input : constant VString := +"aoc_2021_08.txt";
  --
  c, sep : Character;
  f : File_Type;
  first_line : Boolean := True;
  size : Integer;
  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  --  verbose : constant Boolean := True;
begin
  --
  --  Part 1 : count unique patterns (2 segments for digit '1',
  --           3 for '7', 4 for '4', 7 for '8')
  --
  r (1) := 0;
  Open (f, input);
  first_line := True;
  while not End_Of_File (f) loop
    if first_line or End_Of_Line (f) then
      --  Skip the training message
      for i in 1 .. 61 loop
        Get (f, sep);
      end loop;
    end if;
    first_line := False;
    size := 0;
    loop
      Get (f, c);
      exit when c = ' ';
      size := size + 1;
      exit when End_Of_Line (f);
    end loop;
    if size = 2 or size = 3 or size = 4 or size = 7 then
      r (1) := r (1) + 1;
    end if;
  end loop;
  Close (f);
  r (2) := 0;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: : " & r (1));
    Put_Line (+"Part 2: : " & r (2));
    --  Part 1: validated by AoC: 440
    --  Part 2: validated by AoC:
  end if;
end AoC_2021_08;
