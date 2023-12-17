--  Solution to Advent of Code 2023, Day 14
-------------------------------------------
--  Parabolic Reflector Dish
--
--  https://adventofcode.com/2023/day/14
--  Copy of questions in: aoc_2023_14_questions.txt
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

procedure AoC_2023_14 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 10;
  input_name : constant VString := +"aoc_2023_14"; n : constant := 100;
  --

  type Map_Type is array (1 .. n, 1 .. n) of Character;

  map_data, map : Map_Type;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for i in 1 .. n loop
      for j in 1 .. n loop
        Get (f, map_data (i, j));
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer;

  function Count_Load return Integer is
    res : Integer := 0;
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop
        if map (i, j) = 'O' then
          res := res + n - i + 1;
        end if;
      end loop;
    end loop;
    return res;
  end Count_Load;

  procedure Roll_North is
    iii : Integer;
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop
        if map (i, j) = 'O' then
          --  Roll the rock!
          iii := 0;
          for ii in reverse 1 .. i - 1 loop
            if map (ii, j) = '.' then
              iii := ii;
            else
              exit;
            end if;
          end loop;
          if iii /= 0 then
            map (iii, j) := 'O';
            map (i, j)   := '.';
          end if;
        end if;
      end loop;
    end loop;
  end Roll_North;

  procedure Roll_South is
    iii : Integer;
  begin
    for i in reverse 1 .. n loop
      for j in 1 .. n loop
        if map (i, j) = 'O' then
          --  Roll the rock!
          iii := 0;
          for ii in i + 1 .. n loop
            if map (ii, j) = '.' then
              iii := ii;
            else
              exit;
            end if;
          end loop;
          if iii /= 0 then
            map (iii, j) := 'O';
            map (i, j)   := '.';
          end if;
        end if;
      end loop;
    end loop;
  end Roll_South;

  procedure Roll_East is
    jjj : Integer;
  begin
    for j in reverse 1 .. n loop
      for i in 1 .. n loop
        if map (i, j) = 'O' then
          --  Roll the rock!
          jjj := 0;
          for jj in j + 1 .. n loop
            if map (i, jj) = '.' then
              jjj := jj;
            else
              exit;
            end if;
          end loop;
          if jjj /= 0 then
            map (i, jjj) := 'O';
            map (i, j)   := '.';
          end if;
        end if;
      end loop;
    end loop;
  end Roll_East;

  procedure Roll_West is
    jjj : Integer;
  begin
    for j in 1 .. n loop
      for i in 1 .. n loop
        if map (i, j) = 'O' then
          --  Roll the rock!
          jjj := 0;
          for jj in reverse 1 .. j - 1 loop
            if map (i, jj) = '.' then
              jjj := jj;
            else
              exit;
            end if;
          end loop;
          if jjj /= 0 then
            map (i, jjj) := 'O';
            map (i, j)   := '.';
          end if;
        end if;
      end loop;
    end loop;
  end Roll_West;

  procedure Do_Part_1 is
  begin
    map := map_data;
    Roll_North;
    r (part_1) := Count_Load;
  end Do_Part_1;

  verbose : constant Boolean := False;

  procedure Do_Part_2 is

    --  Empirical constants:
    cycle_observation_rounds : constant := 120;
    load_max : constant := 120_000;

    --  `mem` is used for detecting cycles in load values.
    --  Note (possible trap in this puzzle): we are
    --  *not* interested in cycles in rock layout or
    --  in loads measured within a full rotation!
    --
    mem : array (1 .. load_max) of Natural;

    step : Natural := 0;
    cycle : Positive;
    overall_cycle : Positive := 1;

    total : constant := 1_000_000_000;
    rest : Integer;

    procedure Check is
      l : Integer;
    begin
      step := step + 1;
      l := Count_Load;
      if mem (l) in 1 .. step - 1 then
        cycle := step - mem (l);
        overall_cycle := LCM (overall_cycle, cycle);
        if verbose then
          Put_Line
            (+"  Step: " & step &
              "  load: " & l &
              "  deja vu at step: " & mem (l) &
              "; length " & cycle);
        end if;
      end if;
      mem (l) := step;
    end Check;

  begin
    map := map_data;
    for i in mem'Range loop
      mem (i) := 0;
    end loop;
    if verbose then
      Put_Line ("Check cycles");
    end if;
    for count in 1 .. cycle_observation_rounds loop
      Roll_North;
      Roll_West;
      Roll_South;
      Roll_East;
      Check;
    end loop;
    if verbose then
      Put_Line (+"Overall cycle: " & overall_cycle);
    end if;
    rest := total - cycle_observation_rounds;
    rest := rest mod overall_cycle;  --  Now we warp x * `overall_cycle`
    if verbose then
      Put_Line ("Remaining steps : " & rest'Image);
    end if;
    for count in 1 .. rest loop
      Roll_North;
      Roll_West;
      Roll_South;
      Roll_East;
    end loop;
    r (part_2) := Count_Load;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin

  Read_Data;
  Do_Part_1;
  Do_Part_2;

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
    --  Part 1: validated by AoC: 113078
    --  Part 2: validated by AoC: 94255
  end if;
end AoC_2023_14;
