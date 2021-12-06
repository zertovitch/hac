--  Solution to Advent of Code 2021, Day 6
------------------------------------------
--  Lanternfish
--
--  https://adventofcode.com/2021/day/6
--  Copy of questions in: aoc_2021_06_questions.txt
--
with HAL;  --  For a build with "full Ada": files hal*.ad* are in ../../../src

procedure AoC_2021_06 is
  use HAL;
  sep : Character;
  i : Integer;
  res : array (1 .. 2) of Integer;
  f : File_Type;
  type Lanternfish_Population is array (0 .. 8) of Natural;
  pop_init, pop : Lanternfish_Population;
  tmp : Natural;
  days : array (1 .. 2) of Positive;
  --
  input : constant VString := +"aoc_2021_06.txt";
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
begin
  for j in 0 .. 7 loop
    pop_init (j) := 0;
  end loop;
  Open (f, input);
  loop
    Get (f, i);
    pop_init (i) := pop_init (i) + 1;
    exit when End_Of_File (f);
    Get (f, sep);
  end loop;
  Close (f);
  days (1) := 80;
  days (2) := 256;
  --
  for part in 1 .. 2 loop
    pop := pop_init;
    for d in 1 .. days (part) loop
      tmp := pop (0);
      for j in 0 .. 7 loop
        pop (j) := pop (j + 1);
      end loop;
      pop (6) := pop (6) + tmp;
      pop (8) := tmp;
    end loop;
    res (part) := 0;
    for j in 0 .. 8 loop
      res (part) := res (part) + pop (j);
    end loop;
  end loop;
  --
  if compiler_test_mode then
    if res (1) /= Integer_Value (Argument (1)) or
       res (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: : " & res (1));
    Put_Line (+"Part 2: : " & res (2));
    --  Part 1: validated by AoC: 388419
    --  Part 2: validated by AoC: 1740449478328
  end if;
end AoC_2021_06;
