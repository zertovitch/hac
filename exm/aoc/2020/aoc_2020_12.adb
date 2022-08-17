--  Solution to Advent of Code 2020, Day 12
-------------------------------------------
--  Rain Risk
--
--  https://adventofcode.com/2020/day/12
--
--  Compute destination of a ship following navigation
--  instructions (interpreted in two different ways).
--
with HAT;  --  For a build with "full Ada": files HAT*.ad* are in ../../../src

procedure AoC_2020_12 is
  use HAT;
  --
  function D2R (a : Real) return Real is
  begin
    return (Pi / 180.0) * a;
  end D2R;
  --
  procedure Rotate (x, y : in out Real; a : Real) is
    nx : Real;
  begin
    nx := Cos (a) * x - Sin (a) * y;
    y  := Sin (a) * x + Cos (a) * y;
    x  := nx;
  end Rotate;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  c : Character;
  i, res : Integer;
  wx, wy, a, x, y, d, res_r : Real;
  f : File_Type;
begin
  for part in 1 .. 2 loop
    Open (f, "aoc_2020_12.txt");
    x := 0.0;
    y := 0.0;
    a := 0.0;
    wx := 10.0;
    wy :=  1.0;
    while not End_Of_File (f) loop
      Get (f, c);
      Get (f, i);
      d := Real (i);
      if part = 1 then
        case c is
          --  Move the ship directly.
          when 'N' => y := y + d;
          when 'S' => y := y - d;
          when 'E' => x := x + d;
          when 'W' => x := x - d;
          when 'L' => a := a + D2R (d);
          when 'R' => a := a - D2R (d);
          when 'F' =>
             x := x + d * Cos (a);
             y := y + d * Sin (a);
          when others => null;
        end case;
      else
        case c is
          --  Move a waypoint which is relative to the ship's position.
          when 'N' => wy := wy + d;
          when 'S' => wy := wy - d;
          when 'E' => wx := wx + d;
          when 'W' => wx := wx - d;
          when 'L' => Rotate (wx, wy, +D2R (d));
          when 'R' => Rotate (wx, wy, -D2R (d));
          when 'F' =>
             --  Move the ship forward to the waypoint.
             x := x + d * wx;
             y := y + d * wy;
          when others => null;
        end case;
      end if;
    end loop;
    Close (f);
    res_r := abs x + abs y;
    res := Integer (res_r);
    if compiler_test_mode then
      if res /= Integer_Value (Argument (part)) then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
    else
      Put_Line (
        +"Part " & part &
        ": Manhattan distance of the ship to (0,0): " &
        res & "  (" & res_r & ')');
    end if;
  end loop;
  --  Part 1: validated by AoC: 1631
  --  Part 2: validated by AoC: 58606
end AoC_2020_12;
