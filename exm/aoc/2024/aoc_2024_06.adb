--  Solution to Advent of Code 2024, Day 6
-------------------------------------------
--  Guard Gallivant
--
--  https://adventofcode.com/2024/day/6
--  Copy of questions in: aoc_2024_06_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_06 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 10;
  input_name : constant VString := +"aoc_2024_06"; n : constant := 130;

  map : array (1 .. n, 1 .. n) of Character;

  d0 : Direction;
  x0, y0 : Positive;

  r : array (Part_Type) of Integer;

  procedure Read_Data is
    f : File_Type;
    c : Character;
  begin
    Open (f, input_name & ".txt");
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        Get (f, c);
        map (x, y) := c;
        if c = '^' then
          x0 := x;
          y0 := y;
          d0 := north;
        end if;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  verbose : constant Boolean := False;

  visited : array (1 .. n, 1 .. n) of Natural;

  procedure Show_Map (ox, oy : Integer) is
  begin
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        if visited (x, y) > 0 then
          Put ('X');
        elsif ox = x and then oy = y then
          Put ('O');
        else
          Put (map (x, y));
        end if;
      end loop;
      New_Line;
    end loop;
    Put (r (part_2));
    New_Line;
  end Show_Map;

  procedure Do_Part_1 is
    x, y, nx, ny : Integer;
    d : Direction;
  begin
    for yy in 1 .. n loop
      for xx in 1 .. n loop
        visited (xx, yy) := 0;
      end loop;
    end loop;

    x := x0;
    y := y0;
    d := d0;
    loop
      visited (x, y) := visited (x, y) + 1;
      case d is
        when north => nx := x; ny := y + 1;
        when east  => nx := x + 1; ny := y;
        when south => nx := x; ny := y - 1;
        when west  => nx := x - 1; ny := y;
      end case;
      exit when nx not in 1 .. n or else ny not in 1 .. n;
      if map (nx, ny) = '#' then
        d := Turn_Right (d);
      else
        x := nx;
        y := ny;
      end if;
    end loop;

    for yy in 1 .. n loop
      for xx in 1 .. n loop
        if visited (xx, yy) > 0 then
          r (part_1) := r (part_1) + 1;
        end if;
      end loop;
    end loop;

    if verbose then
      Show_Map (0, 0);
    end if;
  end Do_Part_1;

  procedure Do_Part_2 is
    x, y, nx, ny : Integer;
    d : Direction;
  begin

    for obs_x in 1 .. n loop
      for obs_y in 1 .. n loop
        if map (obs_x, obs_y) = '.' then
          --  Free cell: set obstacle here.

          for yy in 1 .. n loop
            for xx in 1 .. n loop
              visited (xx, yy) := 0;
            end loop;
          end loop;

          x := x0;
          y := y0;
          d := d0;
          loop
            visited (x, y) := visited (x, y) + 1;
            if visited (x, y) > 4 then
              --  We arrived on this cell at least twice from the same direction.
              r (part_2) := r (part_2) + 1;
              exit;
            end if;
            case d is
              when north => nx := x; ny := y + 1;
              when east  => nx := x + 1; ny := y;
              when south => nx := x; ny := y - 1;
              when west  => nx := x - 1; ny := y;
            end case;
            exit when nx not in 1 .. n or else ny not in 1 .. n;
            if map (nx, ny) = '#' or else (nx = obs_x and then ny = obs_y) then
              d := Turn_Right (d);
            else
              x := nx;
              y := ny;
            end if;
          end loop;

          if verbose then
            Show_Map (obs_x, obs_y);
          end if;

        end if;

      end loop;
    end loop;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Do_Part_1;
  if not compiler_test_mode then
    Do_Part_2;
  end if;

  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 5461
    --  Part 2: validated by AoC: 1836
  end if;
end AoC_2024_06;
