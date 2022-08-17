--  Solution to Advent of Code 2021, Day 11
-------------------------------------------
--  Dumbo Octopus
--
--  https://adventofcode.com/2021/day/11
--  Copy of questions in: aoc_2021_11_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_11 is
  use HAT;
  input : constant VString := +"aoc_2021_11.txt";
  sx : constant := 10;
  sy : constant := 10;
  map : array (1 .. sx, 1 .. sy) of Natural;
  flashed : array (1 .. sx, 1 .. sy) of Boolean;
  --
  procedure Increase (x, y : Integer) is
  begin
    map (x, y) := map (x, y) + 1;
    if map (x, y) > 9 and then not flashed (x, y) then
      flashed (x, y) := True;
      for yy in y - 1 .. y + 1 loop
        for xx in x - 1 .. x + 1 loop
          if xx in 1 .. sx and then yy in 1 .. sy and then map (xx, yy) <= 9 then
            Increase (xx, yy);
          end if;
        end loop;
      end loop;
    end if;
   end Increase;
  --
  c : Character;
  step, count : Natural;
  f : File_Type;
  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Open (f, input);
  for y in 1 .. sy loop
    for x in 1 .. sx loop
      Get (f, c);
      map (x, y) := Ord (c) - Ord ('0');
    end loop;
  end loop;
  Close (f);
  r (1) := 0;
  step := 0;
  loop
    step := step + 1;
    for y in 1 .. sy loop
      for x in 1 .. sx loop
        flashed (x, y) := False;
      end loop;
    end loop;
    for y in 1 .. sy loop
      for x in 1 .. sx loop
        Increase (x, y);
      end loop;
    end loop;
    --
    count := 0;
    for y in 1 .. sy loop
      for x in 1 .. sx loop
        if map (x, y) > 9 then
          map (x, y) := 0;
          count := count + 1;
        end if;
      end loop;
    end loop;
    if step <= 100 then
       r (1) := r (1) + count;
    end if;
    if count = sx * sy then
      r (2) := step;
      exit;
    end if;
  end loop;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: Total flashes in the first 100 steps: " & r (1));
    Put_Line (+"Part 2: First step during which all octopuses flash: " & r (2));
    --  Part 1: validated by AoC: 1679
    --  Part 2: validated by AoC: 519
  end if;
end AoC_2021_11;
