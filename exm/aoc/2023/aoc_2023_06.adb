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

  function Find_Records (time, distance : Positive) return Natural is
    rt : constant Real := Real (time);
    rd : constant Real := Real (distance);
    sqrt_discriminant : constant Real := Sqrt (rt * rt - 4.0 * rd);
    rt1 : constant Real := Max (0.0, (rt - sqrt_discriminant) * 0.5);
    rt2 : constant Real := Min (rt,  (rt + sqrt_discriminant) * 0.5);
  begin
    --  Brute force:
    --     for t in 0 .. time loop
    --       if (time-t) * t > distance then
    --         n := n + 1;
    --       end if;
    --     end loop;
    --     return n;
    return Natural (rt2 - 0.5) - Natural (rt1 + 0.5) + 1;
  end Find_Records;

  r : array (Part_Type) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin

  r (part_1) :=
    --  Example:
    --     Find_Records  (7,   9) *
    --     Find_Records (15,  40) *
    --     Find_Records (30, 200);
    Find_Records (62, 644) *
    Find_Records (73, 1023) *
    Find_Records (75, 1240) *
    Find_Records (65, 1023);

  r (part_2) := Find_Records (62737565, 644102312401023);

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
    --  Part 1: validated by AoC: .
    --  Part 2: validated by AoC: .
  end if;
end AoC_2023_06;
