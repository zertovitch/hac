--  Solution to Advent of Code 2022, Day 18
-------------------------------------------
--  Boiling Boulders
--
--  https://adventofcode.com/2022/day/18
--  Copy of questions in: aoc_2022_18_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_18 is
  use HAT;

  subtype The_Range is Integer range -1 .. 20;

  type Big_Cube is array (The_Range, The_Range, The_Range) of Natural;

  air   : constant := 0;
  lava  : constant := 1;

  procedure Clear (bc : in out Big_Cube) is
  begin
    for x in Big_Cube'Range (1) loop
      for y in Big_Cube'Range (2) loop
        for z in Big_Cube'Range (3) loop
          bc (x, y, z) := air;
        end loop;
      end loop;
    end loop;
  end Clear;

  cubes_lava, contacts_lava : Natural := 0;
  cubes_air, inner_faces_air : Natural := 0;

  x, y, z : Natural;
  surface_area_lava, surface_area_air : Natural;

  space : Big_Cube;

  procedure Flood_Fill (x, y, z : Integer) is
  begin
    if x in The_Range
      and then y in The_Range
      and then z in The_Range
      and then space (x, y, z) = air
    then
      space (x, y, z) := lava;
      Flood_Fill (x - 1, y, z);
      Flood_Fill (x + 1, y, z);
      Flood_Fill (x, y - 1, z);
      Flood_Fill (x, y + 1, z);
      Flood_Fill (x, y, z - 1);
      Flood_Fill (x, y, z + 1);
    end if;
  end Flood_Fill;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;
  c1, c2 : Character;
  f : File_Type;

begin
  r (1) := 0;
  r (2) := 0;
  Clear (space);
  Open (f, "aoc_2022_18.txt");  -- aoc_2022_18
Read_Data :
  while not End_Of_File (f) loop
    Get (f, x);
    Get (f, c1);
    Get (f, y);
    Get (f, c2);
    Get (f, z);
    if space (x, y, z) = lava then
      Put ("Duplicate!");
    end if;
    space (x, y, z) := lava;
    cubes_lava := cubes_lava + 1;
    --  Count the faces of each lava cube that is
    --  connected to the new lava cube.
    --  Each square is counted once.
    contacts_lava := contacts_lava +
      space (x - 1, y, z) + space (x + 1, y, z) +
      space (x, y - 1, z) + space (x, y + 1, z) +
      space (x, y, z - 1) + space (x, y, z + 1);
  end loop Read_Data;
  Close (f);
  surface_area_lava := 6 * cubes_lava - 2 * contacts_lava;
  r (1) := surface_area_lava;

  Flood_Fill (-1, -1, -1);
  --  Now the only air remaining is trapped inside the lava droplet.
  for x in Big_Cube'Range (1) loop
    for y in Big_Cube'Range (2) loop
      for z in Big_Cube'Range (3) loop
        if space (x, y, z) = air then
          cubes_air := cubes_air + 1;
          --  Count the faces of each air cube that is
          --  connected to another air cube.
          --  Each square is counted twice.
          inner_faces_air := inner_faces_air + 6 -
            (space (x - 1, y, z) + space (x + 1, y, z) +
             space (x, y - 1, z) + space (x, y + 1, z) +
             space (x, y, z - 1) + space (x, y, z + 1));
        end if;
      end loop;
    end loop;
  end loop;
  surface_area_air := (6 * cubes_air - inner_faces_air);
  r (2) :=  surface_area_lava  - surface_area_air;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: surface area of lava . . . . . : " & r (1));
    Put_Line (+"Part 2: exterior surface area of lava  : " & r (2));
    --  Part 1: validated by AoC: 3374
    --  Part 2: validated by AoC: 2010
  end if;
end AoC_2022_18;
