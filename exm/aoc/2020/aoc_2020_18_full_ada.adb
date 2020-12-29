--  Solution to Advent of Code 2020, Day 18
-------------------------------------------
--  Operation Order
--
--  https://adventofcode.com/2020/day/18
--
with Ada.Text_IO, Interfaces;
with AoC_2020_18_Weird_Formulas;  --  Variant of MathPaqs' Formulas.

procedure AoC_2020_18_full_Ada is
  use Ada.Text_IO, Interfaces;
  --  Set `*` as a fake `-`, in parsing and in evaluation.
  package WF_1 is
    new AoC_2020_18_Weird_Formulas (Long_Float,
      Plus      => "+", Minus      => "*", Times      => "*",
      plus_char => '+', minus_char => '*', times_char => '_'
    );
  --  Swap `+` and `*`, in parsing and in evaluation.
  package WF_2 is
    new AoC_2020_18_Weird_Formulas (Long_Float,
      Plus      => "*", Minus      => "-", Times      => "+",
      plus_char => '*', minus_char => '-', times_char => '+'
    );
  --
  sum : Integer_64;
  f : File_Type;
begin
  for part in 1 .. 2 loop
    Open (f, In_File, "aoc_2020_18.txt");
    sum := 0;
    while not End_Of_File (f) loop
      sum := sum + Integer_64 (
        (if part = 1 then WF_1.Evaluate (WF_1.Parse (Get_Line (f)))
                     else WF_2.Evaluate (WF_2.Parse (Get_Line (f)))));
    end loop;
    Close (f);
    Put_Line ("Part" & part'Image & ": sum is" & sum'Image);
    --  Validated by AoC:  14006719520523
    --  Validated by AoC: 545115449981968
  end loop;
end AoC_2020_18_full_Ada;
