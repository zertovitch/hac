--  Solution to Advent of Code 2020, Day 3
------------------------------------------
--
--  Count trees (#) at integer locations, on a straight line
--  going through a map.
--  The line has a given rational slope.
--
--  https://adventofcode.com/2020/day/3
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_03 is
  n : constant VString := +"aoc_2020_03.txt";
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
  f : File_Type;
begin
  Open (f, n);
  for i in 1 .. i_max loop
    for j in 0 .. j_max loop
      Get (f, map (i, j));
    end loop;
  end loop;
  Close (f);
  --
  --
  Put_Line (+"    " & Trees (1, 1));
  Put_Line (+" a) " & Trees (1, 3));
  Put_Line (+"    " & Trees (1, 5));
  Put_Line (+"    " & Trees (1, 7));
  Put_Line (+"    " & Trees (2, 1));
  --
  Put_Line (+" b) " & Trees (1, 1) * Trees (1, 3) *
                      Trees (1, 5) * Trees (1, 7) *
                      Trees (2, 1));
end AoC_2020_03;
