--  Solution to Advent of Code 2020, Day 3
------------------------------------------
--  Toboggan Trajectory
--
--  Count trees (#) at integer locations, on a straight line
--  going through a map.
--  The line has a given rational slope.
--
--  https://adventofcode.com/2020/day/3
--
with HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2020_03 is
  use HAT, Interfaces;

  i_max : constant := 323;  --  1-based
  j_max : constant := 30;   --  0-based
  --
  --  n : VString := +"example_03.txt";
  --  i_max : constant := 11;
  --  j_max : constant := 10;
  --
  map : array (1 .. i_max, 0 .. j_max) of Character;
  --
  --  Count trees on a trajectory with a rational slope (y / x).
  --
  function Trees (y, x : Positive) return Natural is
    ii : Positive := 1;
    jj, t : Natural := 0;
  begin
    for i in 1 .. i_max loop
      if map (ii, jj) = '#' then
        t := t + 1;
      end if;
      jj := (jj + x) mod (j_max + 1);  --  Map is periodic horizontally.
      ii :=  ii + y;
      exit when ii > i_max;
    end loop;
    return t;
  end Trees;
  --
  test_mode : constant Boolean := Argument_Count >= 2;
  f : File_Type;
  prod : Integer_64;
begin
  Open (f, "aoc_2020_03.txt");
  for i in 1 .. i_max loop
    for j in 0 .. j_max loop
      Get (f, map (i, j));
    end loop;
  end loop;
  Close (f);
  --
  prod := Integer_64 (Trees (1, 1)) *
          Integer_64 (Trees (1, 3)) *
          Integer_64 (Trees (1, 5)) *
          Integer_64 (Trees (1, 7)) *
          Integer_64 (Trees (2, 1));
  if test_mode then
    if    Trees (1, 3) /= Integer_Value (Argument (1))
       or prod /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+" a) Trees met with slope 1/3: " & Trees (1, 3));
    Put_Line (+" b) Product of total met trees with different slopes:" & Integer_64'Image (prod));
    --  Part 1: validated by AoC: 218
    --  Part 2: validated by AoC: 3847183340
  end if;
end AoC_2020_03;
