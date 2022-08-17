--  Solution to Advent of Code 2020, Day 15
-------------------------------------------
--  Rambunctious Recitation
--
--  https://adventofcode.com/2020/day/15
--
--  Part 1 only.
--  If we increase HAC's stack a bit, we can do both parts.
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_15 is
  type Preamble is array (1 .. 6) of Natural;
  --
  procedure Play (pre : Preamble; start : Positive; puzzle_id : Positive) is
    compiler_test_mode : constant Boolean := Argument_Count >= 8;
    stop : constant := 2020;
    --  We memorize turn of spoken number (if any), indexed by spoken number.
    mem : array (0 .. stop) of Natural;
    not_seen : constant := 0;
    prev, curr : Natural;
    T1, T2 : Time;
  begin
    T1 := Clock;
    for i in 0 .. stop loop
      mem (i) := not_seen;
    end loop;
    for i in 1 .. start - 2 loop
      mem (pre (i)) := i;
    end loop;
    prev := pre (start - 1);
    for i in start .. stop loop
      if mem (prev) = not_seen then
        curr := 0;
      else
        curr := (i - 1) - mem (prev);  --  "Age"
      end if;
      if compiler_test_mode then
        if i = 2020 then
          if curr /= Integer_Value (Argument (puzzle_id)) then
            Set_Exit_Status (1);  --  Compiler test failed.
          end if;
        end if;
      else
        if i = 2020 or i = stop then
          Put (i); Put (" : "); Put (curr, 0); New_Line;
        end if;
      end if;
      mem (prev) := i - 1;
      prev := curr;
    end loop;
    T2 := Clock;
    if not compiler_test_mode then
      Put_Line (+"----   Computation time: " & (T2 - T1));
      New_Line;
    end if;
  end Play;
  --
  example : array (1 .. 7) of Preamble;
  puzzle : Preamble;
begin
  --  No aggregates in HAC 0.083, we need to fill arrays element by element ...
  example (1) (1) := 0; example (1) (2) := 3; example (1) (3) := 6;  --  2020th number is 436
  example (2) (1) := 1; example (2) (2) := 3; example (2) (3) := 2;  --  2020th number is 1
  example (3) (1) := 2; example (3) (2) := 1; example (3) (3) := 3;  --  2020th number is 10
  example (4) (1) := 1; example (4) (2) := 2; example (4) (3) := 3;  --  2020th number is 27
  example (5) (1) := 2; example (5) (2) := 3; example (5) (3) := 1;  --  2020th number is 78
  example (6) (1) := 3; example (6) (2) := 2; example (6) (3) := 1;  --  2020th number is 438
  example (7) (1) := 3; example (7) (2) := 1; example (7) (3) := 2;  --  2020th number is 1836
  --
  puzzle (1) := 15;
  puzzle (2) := 12;
  puzzle (3) :=  0;
  puzzle (4) := 14;
  puzzle (5) :=  3;
  puzzle (6) :=  1;
  --
  for i in 1 .. 7 loop
    Play (example (i), 4, i);
  end loop;
  Play (puzzle, 7, 8);
end AoC_2020_15;
