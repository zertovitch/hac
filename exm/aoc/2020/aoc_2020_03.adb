--  Solution to Advent of Code 2020, Day 3
------------------------------------------
--
--  Count trees (#) at integer locations, on line through a map.
--  The line has a given rational slope.
--
--  https://adventofcode.com/2020/day/3
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_03 is
  n : constant VString := +"aoc_2020_03.txt";
  itot  : constant := 323;
  jtot  : constant := 31;
  jtot1 : constant := 30;
  --
  --  n : VString := +"example_03.txt";
  --  itot  : constant := 11;
  --  jtot  : constant := 11;
  --  jtot1 : constant := 10;
  --
  map : array (1 .. itot, 0 .. jtot1) of Character;
  f : File_Type;
  --
  --  Count trees on a trajectory with a rational slope (x / y).
  --
  function Check (y, x : Positive) return Positive is
    trees : Natural := 0;
    ii : Positive := 1;
    jj : Natural := 0;
  begin
    for i in 1 .. itot loop
      if map (ii, jj) = '#' then
        trees := trees + 1;
      end if;
      jj := (jj + x) mod jtot;  --  Map is periodic horizontally.
      ii := ii + y;
      exit when ii > itot;
    end loop;
    return trees;
  end Check;
  --
begin
  Open (f, n);
  for i in 1 .. itot loop
    for j in 0 .. jtot1 loop
      Get (f, map (i, j));
    end loop;
    if i < itot then Skip_Line (f); end if;
  end loop;
  Close (f);
  --
  --
  Put_Line (+"    " & Check (1, 1));
  Put_Line (+" a) " & Check (1, 3));
  Put_Line (+"    " & Check (1, 5));
  Put_Line (+"    " & Check (1, 7));
  Put_Line (+"    " & Check (2, 1));
  --
  Put_Line (+" b) " & Check (1, 1) * Check (1, 3) * Check (1, 5) * Check (1, 7) * Check (2, 1));
end AoC_2020_03;
