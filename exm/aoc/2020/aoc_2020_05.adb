--  Solution to Advent of Code 2020, Day 5
-------------------------------------------
--  Binary Boarding
--
--  https://adventofcode.com/2020/day/5
--
--  Binary space partition in the plane for
--  locating seats.
--    F means "front", B means "back",
--    L means "left", and R means "right".
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_05 is
  f : File_Type;
  x : Character;
  max, id, r, c, b : Integer;
  test_mode : constant Boolean := Argument_Count >= 1;
begin
  max := 0;
  Open (f, "aoc_2020_05.txt");
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
    exit when End_Of_File (f);
    Skip_Line (f);
  end loop;
  Close (f);
  if test_mode then
    if max /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Maximum seat Id in the plane = " & max);
  end if;
  --  Part 1: validated by AoC: 835
  --  Part 2: validated by AoC: 649
end AoC_2020_05;
