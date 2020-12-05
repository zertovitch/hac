--  Solution to Advent of Code 2020, Day 5
-------------------------------------------
--
--  Binary Boarding
--
--  https://adventofcode.com/2020/day/5
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_05 is
  n : constant VString := +"aoc_2020_05.txt";
  f : File_Type;
  x : Character;
  max, id, r, c, b : Integer;
begin
  max := 0;
  Open (f, n);
  while not End_Of_File (f) loop
    r := 0;
    b := 64;
    for i in 1 .. 7 loop
      Get (f, x);
      if x = 'B' then
        r := r + b;
      end if;
      b := b / 2;
    end loop;
    c := 0;
    b := 4;
    for i in 1 .. 3 loop
      Get (f, x);
      if x = 'R' then
        c := c + b;
      end if;
      b := b / 2;
    end loop;
    id := r * 8 + c;
    if id > max then max := id; end if;
    Put_Line (id);
    exit when End_Of_File (f);
    Skip_Line (f);
  end loop;
  Close (f);
  Put_Line (+"Max = " & max);
end AoC_2020_05;
