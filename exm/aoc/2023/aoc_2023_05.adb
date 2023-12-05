--  Solution to Advent of Code 2023, Day 5
------------------------------------------
--  If You Give A Seed A Fertilizer
--
--  https://adventofcode.com/2023/day/5
--  Copy of questions in: aoc_2023_05_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_05 is
  use AoC_Toolbox, HAT;

  type Mapping_Rule is record
    dest_start, source_start, length : Natural;
  end record;

  max_list : constant := 40;

  type Mapping_List is array (1 .. max_list) of Mapping_Rule;

  type Map_Type is record
    top  : Natural;
    list : Mapping_List;
  end record;

  type Relation is
    (seed_to_soil,
     soil_to_fertilizer,
     fertilizer_to_water,
     water_to_light,
     light_to_temperature,
     temperature_to_humidity,
     humidity_to_location);

  map : array (Relation) of Map_Type;

  seed : array (1 .. max_list) of Natural;
  top_seed : Natural;

  procedure Read_Data is
    --  input : constant VString := +"mini.txt";
    input : constant VString := +"aoc_2023_05.txt";
    --
    seed_header : String (1 .. 7);
    f : File_Type;
  begin
    top_seed := 0;
    --
    Open (f, input);
    while not End_Of_File (f) loop
      Get (f, seed_header);
      while not End_Of_Line (f) loop
        top_seed := top_seed + 1;
        Get (f, seed (top_seed));
      end loop;
      Skip_Line (f);
      for r in Relation loop
        map (r).top := 0;
        Skip_Line (f, 2);
        while not End_Of_Line (f) loop
          map (r).top := map (r).top + 1;
          Get (f, map (r).list (map (r).top).dest_start);
          Get (f, map (r).list (map (r).top).source_start);
          Get (f, map (r).list (map (r).top).length);
          exit when End_Of_File (f);
          Skip_Line (f);
        end loop;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Walk_down_Maps (x : in out Integer; start_rule : in Relation) is
    offset : Integer;
  begin
    for r in start_rule .. Relation'Last loop
      for li in 1 .. map (r).top loop
        if x in
          map (r).list (li).source_start ..
          map (r).list (li).source_start + map (r).list (li).length - 1
        then
          offset := x - map (r).list (li).source_start;
          x := map (r).list (li).dest_start + offset;
          exit;
        end if;
      end loop;
    end loop;
  end Walk_down_Maps;

  procedure Walk_up_Maps (x : in out Integer; start_rule : in Relation) is
    offset : Integer;
  begin
    for r in reverse Relation'First .. start_rule loop
      for li in 1 .. map (r).top loop
        if x in
          map (r).list (li).dest_start ..
          map (r).list (li).dest_start + map (r).list (li).length - 1
        then
          offset := x - map (r).list (li).dest_start;
          x := map (r).list (li).source_start + offset;
          exit;
        end if;
      end loop;
    end loop;
  end Walk_up_Maps;

  r : array (Part_Type) of Integer;
  lowest_1, lowest_2 : Integer := Integer'Last;

  procedure Lowest_Location_Part_1 is
    x : Integer;
  begin
    for s in 1 .. top_seed loop
      x := seed (s);
      Walk_down_Maps (x, seed_to_soil);
      lowest_1 := Min (lowest_1, x);
      if s mod 2 = 1 then
        lowest_2 := Min (lowest_2, x);
      end if;
    end loop;
    r (part_1) := lowest_1;
  end Lowest_Location_Part_1;

  procedure Lowest_Location_Part_2 is
    x, y : Integer;
    procedure Touches_a_Map_Range is
      --  Assumption: the optimal path goes through at least one
      --  range of one map. The case where the optimal path goes
      --  with the number unchanged from seed to location is captured by
      --  Lowest_Location_Part_1.
      --
      --  Under the assumption, we conjecture that the optimal path
      --  touches the left bound of at least one of the concerned
      --  ranges including the seed range.
      --  The case with the seed range is treated by Lowest_Location_Part_1.
    begin
      for r in Relation loop
        for li in 1 .. map (r).top loop
          --  1) We check if the left bound (of rule "li" for category "r")
          --     in source can stem from any seed range.
          x := map (r).list (li).source_start;
          if r > Relation'First then
            Walk_up_Maps (x, Relation'Pred (r));
          end if;
          for s in 1 .. top_seed loop
            if s mod 2 = 1 then
              if x in seed (s) .. seed (s) + seed (s + 1) - 1 then
                --  Ok, x is in that seed range.
                --  2) We check if left bound in destination go the maps
                --     down to category "location", like for part 1.
                y := map (r).list (li).dest_start;
                if r < Relation'Last then
                  Walk_down_Maps (y, Relation'Succ (r));
                end if;
                lowest_2 := Min (lowest_2, y);
              end if;
            end if;
          end loop;
        end loop;
      end loop;
    end Touches_a_Map_Range;
  begin
    Touches_a_Map_Range;
    r (part_2) := lowest_2;
  end Lowest_Location_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Lowest_Location_Part_1;
  Lowest_Location_Part_2;
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : " & r (part_1));
    Put_Line (+"Part 2: : " & r (part_2));
    --  Part 1: validated by AoC: 261668924
    --  Part 2: validated by AoC: 24261545
  end if;
end AoC_2023_05;
