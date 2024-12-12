--  Solution to Advent of Code 2024, Day 12
-------------------------------------------
--  Garden Groups
--
--  https://adventofcode.com/2024/day/12
--  Copy of questions in: aoc_2024_12_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_12 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini1"; n : constant := 4;
  --  input_name : constant VString := +"mini2"; n : constant := 5;
  --  input_name : constant VString := +"mini3"; n : constant := 10;
  input_name : constant VString := +"aoc_2024_12"; n : constant := 140;

  map : array (1 .. n, 1 .. n) of Character;

  type Seen_Type is array (1 .. n, 1 .. n) of Boolean;
  seen_clear : Seen_Type;  --  Substitute for (others => (others => False))

  --  Borders of each cell (think of spreadsheet cells' styles):
  --
  type Cell_Border is array (Direction) of Boolean;
  type Border_Type is array (1 .. n, 1 .. n) of Cell_Border;
  border_clear : Border_Type;  --  Substitute for (others => (others => (others => False)))

  r : array (Part_Type) of Integer;

  procedure Read_Data is
    f : File_Type;
    cell_border_clear : Cell_Border;
  begin
    for d in Direction loop
      cell_border_clear (d) := False;
    end loop;
    Open (f, input_name & ".txt");
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        Get (f, map (x, y));
        seen_clear (x, y) := False;
        border_clear (x, y) := cell_border_clear;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part (part : Part_Type) is
    seen : Seen_Type;

    procedure Do_Region (x_start, y_start : Integer; plant : Character) is
      fence, area : Natural := 0;
      border : Border_Type;
      --  Min/Max framing for part 2's search for aligned fences.
      --  The framing speeds up massively the search.
      --  In HAC: part 2's total run time goes from 29 seconds to 1.2 second!
      min_x, max_x : Integer := x_start;
      min_y, max_y : Integer := y_start;

      procedure Flood_Fill (x, y, orig_x, orig_y : Integer; dir : Direction_or_Nil) is
      --  Developped from aoc_2023_18.
      begin
        if x in 1 .. n and then y in 1 .. n and then map (x, y) = plant then
          --  We are still inside the region.
          if not seen (x, y) then
            seen (x, y) := True;
            area := area + 1;
            Flood_Fill (x - 1, y, x, y, west);
            Flood_Fill (x + 1, y, x, y, east);
            Flood_Fill (x, y - 1, x, y, south);
            Flood_Fill (x, y + 1, x, y, north);
          end if;
        else
          --  We have crossed a border: either the map's limits, or to
          --  a region for another plant type.
          fence := fence + 1;
          if part = part_2 then
            border (orig_x, orig_y)(dir) := True;
            min_x := Min (orig_x, min_x);
            max_x := Max (orig_x, max_x);
            min_y := Min (orig_y, min_y);
            max_y := Max (orig_y, max_y);
          end if;
        end if;
      end Flood_Fill;

    begin
      if part = part_2 then
        border := border_clear;
      end if;

      Flood_Fill (x_start, y_start, 0, 0, nil);

      if part = part_2 then

        --  Compute rebates for the bulk discount.
        --  Horizontal fences:
        for y in min_y .. max_y loop
          for x in min_x + 1 .. max_x loop
            if border (x, y)(north) and then border (x - 1, y)(north) then
              fence := fence - 1;
            end if;
            if border (x, y)(south) and then border (x - 1, y)(south) then
              fence := fence - 1;
            end if;
          end loop;
        end loop;

        --  Vertical fences:
        for x in min_x .. max_x loop
          for y in min_y + 1 .. max_y loop
            if border (x, y)(west) and then border (x, y - 1)(west) then
              fence := fence - 1;
            end if;
            if border (x, y)(east) and then border (x, y - 1)(east) then
              fence := fence - 1;
            end if;
          end loop;
        end loop;
      end if;

      r (part) := r (part) + area * fence;

    end Do_Region;

  begin
    seen := seen_clear;

    for x in 1 .. n loop
      for y in 1 .. n loop
        if not seen (x, y) then
          Do_Region (x, y, map (x, y));
        end if;
      end loop;
    end loop;
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Do_Part (part_1);
  if not compiler_test_mode then
    Do_Part (part_2);
  end if;

  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 1319878
    --  Part 2: validated by AoC: 784982
  end if;
end AoC_2024_12;
