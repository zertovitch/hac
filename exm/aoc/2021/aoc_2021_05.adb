--  Solution to Advent of Code 2021, Day 05
-------------------------------------------
--  Hydrothermal Venture
--
--  https://adventofcode.com/2021/day/5
--  Copy of questions in: aoc_2021_05_questions.txt
--
--  HAC 0.098 "nice to have"'s detected in this exercise:
--
--    *     ` hits := (others => (others => 0)); `
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_05 is
  use HAT;
  sep_1, sep_3 : Character;
  sep_2 : String (1 .. 3);
  res : array (1 .. 2) of Integer;
  f : File_Type;
  --
  input : constant VString := +"aoc_2021_05.txt";
  subtype X_Range is Integer range 0 .. 1000;
  subtype Y_Range is Integer range 0 .. 1000;
  x1, x2, xx : X_Range;
  y1, y2, yy : Y_Range;
  len : Natural;
  dx, dy : Integer;
  hits : array (X_Range, Y_Range) of Natural;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;

begin
  for x in X_Range loop
    for y in Y_Range loop
      hits (x, y) := 0;
    end loop;
  end loop;
  --
  for part in 1 .. 2 loop
    Open (f, input);
    while not End_Of_File (f) loop
      Get (f, x1);
      Get (f, sep_1);
      Get (f, y1);
      Get (f, sep_2);
      Get (f, x2);
      Get (f, sep_3);
      Get (f, y2);
      case part is
        when 1 =>
          if x1 = x2 then
            for y in Min (y1, y2) .. Max (y1, y2) loop
              hits (x1, y) := hits (x1, y) + 1;
            end loop;
          elsif y1 = y2 then
            for x in Min (x1, x2) .. Max (x1, x2) loop
              hits (x, y1) := hits (x, y1) + 1;
            end loop;
          end if;
        when 2 =>
          if x1 /= x2 and then y1 /= y2 then
            --  The hydrothermal vent mapping systems assumes a diagonal.
            xx := x1;
            yy := y1;
            dx := (x2 - x1) / abs (x2 - x1);
            dy := (y2 - y1) / abs (y2 - y1);
            len := abs (x2 - x1);
            for draw in 0 .. len loop
              hits (xx, yy) := hits (xx, yy) + 1;
              if draw < len then
                xx := xx + dx;
                yy := yy + dy;
              end if;
            end loop;
          end if;
      end case;
    end loop;
    Close (f);
    --
    res (part) := 0;
    for x in X_Range loop
      for y in Y_Range loop
        if hits (x, y) >= 2 then
          res (part) := res (part) + 1;
        end if;
      end loop;
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
    Put_Line (+"Part 1: crossings of horizontal and vertical lines: " & res (1));
    Put_Line (+"Part 2: crossings, including diagonals: " & res (2));
    --  Part 1: validated by AoC: 6225
    --  Part 2: validated by AoC: 22116
  end if;
end AoC_2021_05;
