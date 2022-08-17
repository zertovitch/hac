--  Solution to Advent of Code 2021, Day 6
------------------------------------------
--  Lanternfish
--
--  https://adventofcode.com/2021/day/6
--  Copy of questions in: aoc_2021_06_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2021_06 is
  timer_max : constant := 8;
  subtype Timer_Range is Integer range 0 .. timer_max;
  use HAT, Interfaces;
  type Lanternfish_Population is array (Timer_Range) of Integer_64;
  pop_init, pop : Lanternfish_Population;
  pop_0 : Integer_64;
  days : array (1 .. 2) of Positive;
  --
  input : constant VString := +"aoc_2021_06.txt";
  sep : Character;
  i : Integer;
  f : File_Type;
  r : array (1 .. 2) of Integer_64;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
begin
  for j in Timer_Range loop
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
  --
  days (1) := 80;
  days (2) := 256;
  for part in 1 .. 2 loop
    pop := pop_init;
    for d in 1 .. days (part) loop
      pop_0 := pop (0);
      for j in 0 .. timer_max - 1 loop
        pop (j) := pop (j + 1);
      end loop;
      pop (6) := pop (6) + pop_0;  --  Life cycle
      pop (timer_max) := pop_0;    --  New fishes
    end loop;
    r (part) := 0;
    for j in Timer_Range loop
      r (part) := r (part) + pop (j);
    end loop;
  end loop;
  --
  if compiler_test_mode then
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    for part in 1 .. 2 loop
      Put_Line (
        +"Part " & part & ": population after " & days (part) &
        " days:" & Integer_64'Image (r (part)));
    end loop;
    --  Part 1: validated by AoC: 388419
    --  Part 2: validated by AoC: 1740449478328
  end if;
end AoC_2021_06;
