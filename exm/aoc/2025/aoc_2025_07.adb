--  Solution to Advent of Code 2025, Day 7
------------------------------------------
--  Laboratories
--
--  https://adventofcode.com/2025/day/7
--  Copy of questions in: aoc_2025_07_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in the upper directory: ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2025.gpr .
with HAT;

procedure AoC_2025_07 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; m : constant := 16; n : constant := 15;
  input_name : constant VString := +"aoc_2025_07"; m : constant := 142; n : constant := 141;

  map : array (1 .. m, 1 .. n) of Character;
  j0 : Positive;

  r : array (Part_Type) of VString;

  procedure Read_Data is
    f : File_Type;
    c : Character;
  begin
    Open (f, input_name & ".txt");
    for i in 1 .. m loop
      for j in 1 .. n loop
        Get (f, c);
        map (i, j) := c;
        if c = 'S' then
          j0 := j;
        end if;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
    total : Integer := 0;
  begin
    map (2, j0) := '|';
    for i in 3 .. m loop
      for j in 1 .. n loop
        if map (i - 1, j) = '|' then
          if map (i, j) = '^' then
            map (i, j - 1) := '|';
            map (i, j + 1) := '|';
            total := total + 1;
          else
            map (i, j) := '|';
          end if;
        end if;
      end loop;
    end loop;
    r (part_1) := +"" & total;
  end Do_Part_1;

  procedure Do_Part_2 is
    beam : array (1 .. m, 1 .. n) of Natural;
    total : Integer := 0;
  begin
    for i in 1 .. m loop
      for j in 1 .. n loop
        beam (i, j) := 0;
      end loop;
    end loop;
    beam (2, j0) := 1;
    for i in 3 .. m loop
      for j in 1 .. n loop
        if beam (i - 1, j) > 0 then
          if map (i, j) = '^' then
            beam (i, j - 1) := beam (i, j - 1) + beam (i - 1, j);
            beam (i, j + 1) := beam (i, j + 1) + beam (i - 1, j);
          else
            beam (i, j) := beam (i, j) + beam (i - 1, j);
          end if;
        end if;
      end loop;
    end loop;
    for j in 1 .. n loop
      total := total + beam (n, j);
    end loop;
    r (part_2) := +"" & total;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 1562.
    --  Part 2: validated by AoC: 24292631346665.
  end if;
end AoC_2025_07;
