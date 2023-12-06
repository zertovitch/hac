--  Solution to Advent of Code 2023, Day 6
------------------------------------------
--  Wait For It
--
--  https://adventofcode.com/2023/day/6
--  Copy of questions in: aoc_2023_06_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_06 is
  use AoC_Toolbox, HAT;

  function Find_Records (total_time, distance : Real) return Natural is
    --  Solutions of:
    --
    --            distance = (total_time - t)  *  t
    --                       ^-time for running   ^-speed
    --
    --  where t is the time of pressing the button
    --
    sqrt_discriminant : constant Real := Sqrt (total_time ** 2 - 4.0 * distance);
    t1 : constant Real := (total_time - sqrt_discriminant) * 0.5;
    t2 : constant Real := (total_time + sqrt_discriminant) * 0.5;
  begin
    return Natural (t2 - 0.5) - Natural (t1 + 0.5) + 1;
  end Find_Records;

  r : array (Part_Type) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin

  r (part_1) :=
    --  Example:
    --     Find_Records  (7.0,   9.0) *
    --     Find_Records (15.0,  40.0) *
    --     Find_Records (30.0, 200.0);
    Find_Records (62.0,  644.0) *
    Find_Records (73.0, 1023.0) *
    Find_Records (75.0, 1240.0) *
    Find_Records (65.0, 1023.0);

  r (part_2) := Find_Records (62737565.0, 644102312401023.0);

  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : " & r (part_1));
    Put_Line (+"Part 2: : " & r (part_2));
    --  Part 1: validated by AoC: 393120
    --  Part 2: validated by AoC: 36872656
  end if;
end AoC_2023_06;
