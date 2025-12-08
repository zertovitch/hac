--  Solution to Advent of Code 2025, Day 4
------------------------------------------
--  Printing Department
--
--  https://adventofcode.com/2025/day/4
--  Copy of questions in: aoc_2025_04_questions.txt
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

procedure AoC_2025_04 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 10; np1 : constant := 11;
  input_name : constant VString := +"aoc_2025_04"; n : constant := 136; np1 : constant := 137;

  type Map_Type is array (0 .. np1, 0 .. np1) of Character;

  map : array (0 .. 1) of Map_Type;

  initial : Map_Type;

  r : array (Part_Type) of VString;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for w in 0 .. np1 loop
      initial (w, 0) := 'W';
      initial (w, np1) := 'W';
      initial (0, w) := 'W';
      initial (np1, w) := 'W';
    end loop;
    while not End_Of_File (f) loop
      for y in 1 .. n loop
        for x in 1 .. n loop
          Get (f, initial (x, y));
        end loop;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
    rolls, res : Natural;
  begin
    res := 0;
    for y in 1 .. n loop
      for x in 1 .. n loop
        rolls := 8;
        if map (0)(x, y) = '@' then
          rolls := 0;
          for xx in -1 .. 1 loop
            for yy in -1 .. 1 loop
              if abs xx + abs yy /= 0 and then map (0)(x + xx, y + yy) = '@' then
                rolls := rolls + 1;
              end if;
            end loop;
          end loop;
        end if;
        if rolls < 4 then
          res := res + 1;
        end if;
      end loop;
    end loop;
    r (part_1) := +"" & res;
  end Do_Part_1;

  procedure Do_Part_2 is
    rolls : Natural;
    m : Natural := 0;
    moved, total : Natural;
  begin
    total := 0;
    loop
      moved := 0;
      map (1 - m) := map (m);  --  Clone current map.
      for y in 1 .. n loop
        for x in 1 .. n loop
          rolls := 8;
          if map (m)(x, y) = '@' then
            rolls := 0;
            for xx in -1 .. 1 loop
              for yy in -1 .. 1 loop
                if abs xx + abs yy /= 0 and then map (m)(x + xx, y + yy) = '@' then
                  rolls := rolls + 1;
                end if;
              end loop;
            end loop;
          end if;
          if rolls < 4 then
            map (1 - m)(x, y) := '.';
            moved := moved + 1;
          end if;
        end loop;
      end loop;
      total := total + moved;
      exit when moved = 0;
      m := 1 - m;
    end loop;
    r (part_2) := +"" & total;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  map (0) := initial;
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
    --  Part 1: validated by AoC: 1320.
    --  Part 2: validated by AoC: 8354.
  end if;
end AoC_2025_04;
