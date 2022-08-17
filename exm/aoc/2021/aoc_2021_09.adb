--  Solution to Advent of Code 2021, Day 9
------------------------------------------
--  Smoke Basin
--
--  First 2021 puzzle with recursion :-)
--
--  https://adventofcode.com/2021/day/9
--  Copy of questions in: aoc_2021_09_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_09 is
  use HAT;
  --
  input : constant VString := +"aoc_2021_09.txt";
  sx : constant := 100;
  sy : constant := 100;
  --
  c : Character;
  f : File_Type;
  r : array (1 .. 2) of Integer;
  map : array (1 .. sx, 1 .. sy) of Natural;
  seen : array (1 .. sx, 1 .. sy) of Boolean;
  size, la1, la2, la3 : Natural := 0;
  --
  --  `Visit` returns the size of the basin around point (x, y).
  --  Side effect: it writes in `seen`.
  --
  function Visit (x, y : Integer) return Natural is
  begin
    if x < 1 or else x > sx or else y < 1 or else y > sy or else seen (x, y) then
      return 0;
    end if;
    seen (x, y) := True;
    if map (x, y) = 9 then
      return 0;
    end if;
    return 1 + Visit (x - 1, y) + Visit (x + 1, y) + Visit (x, y - 1) + Visit (x, y + 1);
  end Visit;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
begin
  Open (f, input);
  for y in 1 .. sy loop
    for x in 1 .. sx loop
      Get (f, c);
      map (x, y) := Ord (c) - Ord ('0');
      seen (x, y) := False;
    end loop;
  end loop;
  Close (f);
  r (1) := 0;
  for y in 1 .. sy loop
    for x in 1 .. sx loop
      if (x = 1  or else map (x - 1, y) > map (x, y)) and then
         (y = 1  or else map (x, y - 1) > map (x, y)) and then
         (x = sx or else map (x + 1, y) > map (x, y)) and then
         (y = sy or else map (x, y + 1) > map (x, y))
      then
         --  We have found a low point - a location (x, y) that is
         --  lower than any of its adjacent locations.
         r (1) := r (1) + map (x, y) + 1;
         size := Visit (x, y);
         if size > la1 then
           la3 := la2;
           la2 := la1;
           la1 := size;
         elsif size > la2 then
           la3 := la2;
           la2 := size;
         elsif size > la3 then
           la3 := size;
         end if;
      end if;
    end loop;
  end loop;
  r (2) := la1 * la2 * la3;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: number of low points: " & r (1));
    Put_Line (+"Part 2: product of the sizes of the 3 largest basins: " & r (2));
    --  Part 1: validated by AoC: 423
    --  Part 2: validated by AoC: 1198704
  end if;
end AoC_2021_09;
