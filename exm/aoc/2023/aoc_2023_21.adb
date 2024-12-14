--  Solution to Advent of Code 2023, Day 21
-------------------------------------------
--  Step Counter
--
--  https://adventofcode.com/2023/day/21
--  Copy of questions in: aoc_2023_21_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in the upper directory (..)
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

with Interfaces;

procedure AoC_2023_21 is

  use AoC_Toolbox, HAT, Interfaces;

  --  input_name : constant VString := +"mini"; n : constant := 11; n7 : constant := 77; steps_part_1 : constant := 6;
  input_name : constant VString := +"aoc_2023_21"; n : constant := 131; n7 : constant := 917; steps_part_1 : constant := 64;

  steps_part_2 : constant := 26501365;

  type Map_Type is array (1 .. n7, 1 .. n7) of Character;

  map : array (Binary) of Map_Type;

  procedure Read_Data is
    c : Character;
    f : File_Type;
    S : Point;
  begin
    Open (f, input_name & ".txt");
    for y in 1 .. n loop
      for x in 1 .. n loop
        Get (f, c);
        if c = 'S' then
          c := '.';
          S.x := x;
          S.y := y;
        end if;
        --  Copy the data on a n*7 x n*7 map.
        --  The 7-fold map is used for extrapolating
        --  further to the infinity...
        for kx in 0 .. 6 loop
          for ky in 0 .. 6 loop
            map (0)(x + n * kx, y + n * ky) := c;
          end loop;
        end loop;
      end loop;
    end loop;
    Close (f);
    map (1) := map (0);
    map (0) (S.x + 3 * n, S.y + 3 * n) := 'O';
  end Read_Data;

  verbosity : constant := 0;
  compiler_test_mode : constant Boolean := Argument_Count >= 1;

  r : array (Part_Type) of Integer_64;

  procedure Solve is
    cur : Binary := 0;  --  Oscillate between two maps (game-of-life-style).

    procedure Step_Aside (x, y : Integer) is
    begin
      if map (1 - cur) (x, y) = '.' then
        map (1 - cur) (x, y) := 'O';
      end if;
    end Step_Aside;

    b_min, b_max : Positive;

    function Count_Garden_Plots return Natural is
      total : Natural := 0;
    begin
      for y in b_min .. b_max loop
        for x in b_min .. b_max loop
          if map (cur) (x, y) = 'O' then
            total := total + 1;
          end if;
        end loop;
      end loop;
      return total;
    end Count_Garden_Plots;

    degree : constant := 2;
    t : Integer_64;
    --  Observations of Count_Garden_Plots every n steps:
    obs : array (0 .. degree) of Integer_64;

  begin
  Macro :
    for macro_step in 0 .. degree loop
      b_min := 1 + n * (2 - macro_step);
      b_max := n * (5 + macro_step);
    Micro :
      for micro_step in 1 .. n loop
        if verbosity >= 2 then
          Put_Line ("------------");
          for y in 1 .. n7 loop
            for x in 1 .. n7 loop
              Put (map (cur) (x, y));
            end loop;
            New_Line;
          end loop;
        end if;
        for y in b_min .. b_max loop
          for x in b_min .. b_max loop
            if map (cur) (x, y) = 'O' then
              Step_Aside (x, y - 1);
              Step_Aside (x, y + 1);
              Step_Aside (x + 1, y);
              Step_Aside (x - 1, y);
              map (1 - cur) (x, y) := '.';
            end if;
          end loop;
        end loop;
        cur := 1 - cur;
        if macro_step = 0 and then micro_step = steps_part_1 then
          r (part_1) := Integer_64 (Count_Garden_Plots);
          if compiler_test_mode then
            return;
          end if;
        end if;
        if (micro_step mod n) = (steps_part_2 mod n) then
          --  We measure the result every n steps for a while.
          obs (macro_step) := Integer_64 (Count_Garden_Plots);
          exit Macro when macro_step = degree;
        end if;
      end loop Micro;
    end loop Macro;
    --
    --  Someone somewhere seems to claim that due to favourable
    --  settings (n being odd, S being at the centre, borders and
    --  the crosss through S being free, moves being only horizontal
    --  or vertical,...), the result is exactly predicted by a
    --  polynomial of degree 2.
    --
    --  We determine p such as:   p (t) = obs (t)  for t = 0, 1, 2
    --
    if verbosity >= 1 then
      for d in 0 .. degree loop
        Put_Line (+"d = " & d & ", observation " & obs (d)'Image);
      end loop;
    end if;
    t := steps_part_2 / n;
    r (part_2) :=
       obs (0) +
      (obs (1) - obs (0)) * t +
      (obs (2) - 2 * obs (1) + obs (0)) * t * (t - 1) / 2;
  end Solve;

  T0 : constant Time := Clock;

begin

  Read_Data;
  Solve;

  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1))) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1)'Image);
    Put_Line (+"Part 2: " & r (part_2)'Image);
    --  Part 1: validated by AoC: 3677.
    --  Part 2: validated by AoC: 609585229256084.
  end if;
end AoC_2023_21;
