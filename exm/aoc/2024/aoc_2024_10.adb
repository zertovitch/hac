--  Solution to Advent of Code 2024, Day 10
-------------------------------------------
--  Hoof It
--
--  https://adventofcode.com/2024/day/10
--  Copy of questions in: aoc_2024_10_questions.txt
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

procedure AoC_2024_10 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2024_10";

  n_max  : constant := 56;

  nx_max : constant := n_max;
  ny_max : constant := n_max;

  subtype Range_X is Integer range 1 .. nx_max;
  subtype Range_Y is Integer range 1 .. ny_max;

  type Map_Type is array (Range_X, Range_Y) of Integer;

  map : Map_Type;
  n : Point;

  procedure Data_Acquisition is
    c : Character;
    f : File_Type;
    x, y : Natural := 0;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      y := y + 1;
      x := 0;
      Get (f, c);
      loop
        x := x + 1;
        map (x, y) := Character'Pos (c) - Character'Pos ('0');
        exit when End_Of_Line (f);
        Get (f, c);
      end loop;
    end loop;
    n.x := x;
    n.y := y;
    Close (f);
  end Data_Acquisition;

  r : array (Part_Type) of Integer;

  --  Walk (part 2: all possible paths) to all possible points with a 9.
  --  Since we always go up one level above, we don't need to mark visited cells.
  --
  procedure Hike (x, y, level : Integer; part : Part_Type) is
  begin
    if x in 1 .. n.x and then y in 1 .. n.y and then level = map (x, y) then
      if level = 9 then
        r (part) := r (part) + 1;
        if part = part_1 then
          map (x, y) := -1;  --  "Erase" the summit (we count only one path to it).
        end if;
      else
        Hike (x - 1, y, level + 1, part);
        Hike (x + 1, y, level + 1, part);
        Hike (x, y - 1, level + 1, part);
        Hike (x, y + 1, level + 1, part);
      end if;
    end if;
  end Hike;

  procedure Find_Hiking_Heads is
    map_orig : constant Map_Type := map;
  begin
    for y in 1 .. n.y loop
      for x in 1 .. n.x loop
        if map (x, y) = 0 then
          --  Part 1: walk to all possible summits.
          Hike (x, y, 0, part_1);
          map := map_orig;
          --  Part 2: walk all possible paths to all possible summits.
          Hike (x, y, 0, part_2);
        end if;
      end loop;
    end loop;
  end Find_Hiking_Heads;

  T0 : constant Time := Clock;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Data_Acquisition;
  Find_Hiking_Heads;

  if compiler_test_mode then
    if r (part_1) /= Integer'Value (To_String (Argument (1))) or
       r (part_2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: score  : " & r (part_1));
    Put_Line (+"Part 2: rating : " & r (part_2));
    --  Part 1: validated by AoC: 737
    --  Part 2: validated by AoC: 1619
  end if;
end AoC_2024_10;
