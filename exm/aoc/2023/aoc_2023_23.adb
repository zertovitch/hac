--  Solution to Advent of Code 2023, Day 23
------------------------------------------
--  A Long Walk
--
--  https://adventofcode.com/2023/day/23
--  Copy of questions in: aoc_2023_23_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_23 is

  use AoC_Toolbox, HAT;

  input_name : VString;
  skip_header : Natural;
  n : Positive;

  max_n : constant := 141;

  type Map_Type is array (1 .. max_n, 1 .. max_n) of Character;

  data : Map_Type;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for skip_it in 1 .. skip_header loop
      Skip_Line (f);
    end loop;
    for y in 1 .. n loop
      for x in 1 .. n loop
        Get (f, data (x, y));
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Show (map : Map_Type) is
  begin
    Put_Line ("Map: -------------------");
    for y in 1 .. n loop
      for x in 1 .. n loop
        Put (map (x, y));
      end loop;
      New_Line;
    end loop;
  end Show;

  r : array (Part_Type) of Integer;

  verbosity : constant := 0;

  procedure Do_Part (part : Part_Type) is

    procedure Walk_from (map : in out Map_Type; sx, sy : Integer; done_so_far : Natural) is

      more : Natural := 0;

      procedure Flood_Fill (x, y : Integer; ice : Character) is

        procedure Go is
        begin
          if x = n - 1 and then y = n then
            map (x, y) := 'E';
            more := more + 1;
            if verbosity >= 1 then
              Put_Line (+"Yuhu " & (done_so_far + more));
              if verbosity >= 2 then
                Show (map);
              end if;
            end if;
            r (part) := Max (r (part), done_so_far + more);
          else
            map (x, y) := 'O';
            more := more + 1;
            Flood_Fill (x - 1, y, '<');
            Flood_Fill (x + 1, y, '>');
            Flood_Fill (x, y - 1, '^');
            Flood_Fill (x, y + 1, 'v');
          end if;
        end Go;

        procedure Copy_and_Go is
          copy : Map_Type := map;
        begin
          copy (x, y) := '.';  --  Open the icy gate...
          Walk_from (copy, x, y, done_so_far + more);
        end Copy_and_Go;

        c : constant Character := map (x, y);

      begin
        if c = '.' then
          Go;
        elsif c = ice
          or else
            (part = part_2
             and then (c = '<' or else c = '>' or else c = '^' or else c = 'v'))
        then
          Copy_and_Go;
          --  Silent alternative: treat this tile as a forest,
          --  continue on the same map and find another path.
        end if;
      end Flood_Fill;

    begin
      Flood_Fill (sx, sy, '$');
    end Walk_from;

    map : Map_Type := data;

  begin
    if verbosity >= 2 then
      Show (map);
    end if;
    r (part) := 0;
    map (2, 1) := 'S';  --  Nice, and prevents going outside...
    Walk_from (map, 2, 2, 0);
  end Do_Part;

  T0 : constant Time := Clock;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;

begin
  if compiler_test_mode then
    --  GNAT runs this program on the actual problem
    --  data in 5 minutes, while HAC takes forever.
    input_name := +"aoc_2023_23_questions";
    skip_header := 16;
    n := 23;
  else
    input_name := +"aoc_2023_23";
    skip_header := 0;
    n := 141;
  end if;

  Read_Data;

  for part in Part_Type loop
    Do_Part (part);
    if verbosity >= 1 then
      Put_Line ("Done for part " & part'Image);
    end if;
  end loop;

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
    --  Part 1: validated by AoC: 2114    (example: 94)
    --  Part 2: validated by AoC: 6322    (example: 154)
  end if;
end AoC_2023_23;
