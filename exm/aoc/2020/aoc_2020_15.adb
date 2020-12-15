--  Solution to Advent of Code 2020, Day 15
-------------------------------------------
--  Rambunctious Recitation
--
--  https://adventofcode.com/2020/day/15
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_15 is
  compiler_test_mode : constant Boolean := Argument_Count >= 8;
  last : constant := 2020;
  type Preamble is array (1 .. 6) of Natural;
  --
  procedure Play (pre : Preamble; start : Positive; puzzle_id : Positive) is
    mem : array (1 .. last) of Integer;
    --
    function Is_new (i, n : Natural) return Boolean is
    begin
      for j in 1 .. i - 2 loop
        if mem (j) = n then return False; end if;
      end loop;
      return True;
    end Is_new;
    --
    function Age (i, n : Natural) return Natural is
    begin
      for j in reverse 1 .. i - 2 loop
        if mem (j) = n then return (i - 1) - j; end if;
      end loop;
      return 0;
    end Age;
  begin
    for i in 1 .. start - 1 loop
      mem (i) := pre (i);
    end loop;
    for i in start .. last loop
      if Is_new (i, mem (i - 1)) then
        mem (i) := 0;
      else
        mem (i) := Age (i, mem (i - 1));
      end if;
    end loop;
    if compiler_test_mode then
      if mem (last) /= Integer_Value (Argument (puzzle_id)) then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
    else
      Put_Line (+"Last number is " & mem (last));
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
