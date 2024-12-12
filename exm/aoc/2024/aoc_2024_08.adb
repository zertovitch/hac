--  Solution to Advent of Code 2024, Day 8
-------------------------------------------
--  Resonant Collinearity
--
--  https://adventofcode.com/2024/day/8
--  Copy of questions in: aoc_2024_08_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_08 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 12;
  input_name : constant VString := +"aoc_2024_08"; n : constant := 50;

  map : array (1 .. n, 1 .. n) of Character;

  type Antenna_Type is record
    x, y : Positive;
    c    : Character;
  end record;

  a : array (1 .. 2500) of Antenna_Type;
  l : Natural := 0;

  r : array (Part_Type) of Integer;

  procedure Read_Data is
    c : Character;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        Get (f, c);
        map (x, y) := c;
        if c in 'a' .. 'z' or c in 'A' .. 'Z' or c in '0' .. '9' then
          l := l + 1;
          a (l).x := x;
          a (l).y := y;
          a (l).c := c;
        end if;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Show_Map is
  begin
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        Put (map (x, y));
      end loop;
      New_Line;
    end loop;
    New_Line;
  end Show_Map;

  verbose : constant Boolean := False;

  procedure Do_Part (part : Part_Type) is

    procedure Plant_Antinode (xa, ya : Integer) is
    begin
      if xa in 1 .. n and then ya in 1 .. n then
        map (xa, ya) := '#';
      end if;
    end Plant_Antinode;

    dx, dy : Integer;

  begin
    for i in 1 .. l loop
      for j in i + 1 .. l loop
        if a (i).c = a (j).c then
          --  Create andinodes:
          dx := a (j).x - a (i).x;
          dy := a (j).y - a (i).y;
          case part is
            when part_1 =>
              Plant_Antinode (a (i).x - dx, a (i).y - dy);
              Plant_Antinode (a (j).x + dx, a (j).y + dy);
            when part_2 =>
              for m in -n .. n loop
                Plant_Antinode (a (j).x + m * dx, a (j).y + m * dy);
              end loop;
          end case;
        end if;
      end loop;
    end loop;

    --  Count the antinode locations:
    for y in 1 .. n loop
      for x in 1 .. n loop
        if map (x, y) = '#' then
          r (part) := r (part) + 1;
        end if;
      end loop;
    end loop;

    if verbose then
      Show_Map;
    end if;
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Do_Part (part_1);
  Do_Part (part_2);
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 409
    --  Part 2: validated by AoC: 1308
  end if;
end AoC_2024_08;
