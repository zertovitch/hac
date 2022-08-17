--  Solution to Advent of Code 2021, Day 1
------------------------------------------
--  Sonar Sweep
--
--  https://adventofcode.com/2021/day/1
--  Copy of questions in: aoc_2021_01_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_01 is
  use HAT;
  f : File_Type;
  depth, depth_p1, depth_p2, inc_a, inc_b, row, sum, sum_p1 : Integer;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
begin
  Open (f, "aoc_2021_01.txt");
  depth_p1 := -1;
  depth_p2 := -1;
  inc_a := 0;
  inc_b := 0;
  row := 0;
  sum := -1;
  while not End_Of_File (f) loop
    Get (f, depth);
    row := row + 1;
    if row > 1 and then depth > depth_p1 then
      inc_a := inc_a + 1;
    end if;
    sum_p1 := sum;
    sum := depth + depth_p1 + depth_p2;
    if row > 3 and then sum > sum_p1 then
      inc_b := inc_b + 1;
    end if;
    depth_p2 := depth_p1;
    depth_p1 := depth;
  end loop;
  Close (f);
  if compiler_test_mode then
    if inc_a /= Integer_Value (Argument (1)) or
       inc_b /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: Depth increases: " & inc_a);
    Put_Line (+"Part 2: Depth increases: " & inc_b);
    --  Part 1: validated by AoC: 1154
    --  Part 2: validated by AoC: 1127
  end if;
end AoC_2021_01;
