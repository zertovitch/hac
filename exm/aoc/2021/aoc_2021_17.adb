--  Solution to Advent of Code 2021, Day 17
-------------------------------------------
--  Trick Shot
--
--  https://adventofcode.com/2021/day/17
--  Copy of questions in: aoc_2021_17_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_17 is
  use HAT;
  --  Input target area: x=155..215, y=-132..-72
  x_min : constant :=  155;
  x_max : constant :=  215;
  y_min : constant := -132;
  y_max : constant :=  -72;

  --  Example: target area: x=20..30, y=-10..-5
  --  x_min : constant :=  20;
  --  x_max : constant :=  30;
  --  y_min : constant := -10;
  --  y_max : constant :=  -5;

  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
  x, y, ym, ymm, vx, vy, hits : Integer;
begin
  ymm := 0;
  hits := 0;
  for vx_0 in 1 .. x_max loop  --  Above x_max you miss the area at first step.
    for vy_0 in y_min .. 200 loop  --  Top value is heuristically searched (stable results).
      vx := vx_0;
      vy := vy_0;
      x := 0;
      y := 0;
      ym := 0;
      loop
        x := x + vx;
        y := y + vy;
        ym := Max (ym, y);
        vx := Max (0, vx - 1);
        vy := vy - 1;
        if x in x_min .. x_max and then y in y_min .. y_max then
          hits := hits + 1;
          if ym > ymm then
            ymm := ym;
            --  Put_Line (+"hit ym =" & ym & "   vx_0 = " & vx_0 & ", vy_0 = " & vy_0);
          end if;
          exit;
        end if;
        exit when y < y_min;
      end loop;
    end loop;
  end loop;
  r (1) := ymm;
  r (2) := hits;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: highest y position reached on a trajectory with a hit:   " & r (1));
    Put_Line (+"Part 2: number of distinct initial velocity values causing hits: " & r (2));
    --  Part 1: validated by AoC: 8646
    --  Part 2: validated by AoC: 5945
  end if;
end AoC_2021_17;
