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

with Interfaces;

procedure AoC_2023_05 is

  use AoC_Toolbox, HAT, Interfaces;

  type Mapping_Rule is record
    dest_start, source_start, length : Integer_64;
  end record;

  max_list : constant := 50;

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

  seed : array (1 .. max_list) of Integer_64;
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

  procedure Walk_down_Maps (x : in out Integer_64; start_rule : in Relation) is
    offset : Integer_64;
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

  procedure Walk_up_Maps (x : in out Integer_64; start_rule : in Relation) is
    offset : Integer_64;
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

  r : array (Part_Type) of Integer_64;
  lowest_1, lowest_2 : Integer_64 := Integer_64'Last;

  procedure Lowest_Location_Part_1 is
    x_down : Integer_64;
  begin
    for s in 1 .. top_seed loop
      x_down := seed (s);
      Walk_down_Maps (x_down, seed_to_soil);
      lowest_1 := Min (lowest_1, x_down);
      if s mod 2 = 1 then
        lowest_2 := Min (lowest_2, x_down);
      end if;
    end loop;
    r (part_1) := lowest_1;
  end Lowest_Location_Part_1;

  procedure Lowest_Location_Part_2 is

    procedure Test (start_point : Integer_64; r : Relation) is

      --  We use the fact that the optimal path (the path that goes from a
      --  seed to the minimal location number) touches the left bound of
      --  at least one range (of those explicitly stated in the Almanac,
      --  including seed ranges, or of implicit ones: for each transition
      --  level, between two explicit ranges, or from 0 to the first range,
      --  or from the last range to the infinity).

      --  Proof (Reductio ad absurdum):
      --  -----
      --         Let s -> L(s) the function that associates stem s
      --         to its location following the Almanac's rules.
      --         Assume we have sm such as L(sm) is minimal, but the path
      --         from sm to L(sm) doesn't touch any left bound.
      --         In that case, sm - 1 is in the same seed range as sm and
      --         in the same seed-to-soil source range as sm; the soil number
      --         for (sm - 1) is the same soil-to-fertilizer source range as
      --         for sm, and so on. The path from (sm - 1) down to the
      --         location number is the same as the path from sm, but shifted
      --         by - 1 at each level.
      --         So, L(sm - 1) = L(sm) - 1.
      --         Therefore, L(sm) is not the claimed minimum. QED.

      x_up, x_down : Integer_64 := start_point;
    begin
      --  1) We check if x_up in source can stem from any seed range.
      --     The test assumes injectivity in the whole mapping.
      --     Otherwise we could have cases where the Walk_up_Maps' result
      --     is in no seed range but some way down from some seed range
      --     could still lead to start_point.
      if r > Relation'First then
        Walk_up_Maps (x_up, Relation'Pred (r));
      end if;
      for s_idx in 1 .. top_seed loop
        if s_idx mod 2 = 1 then
          if x_up in seed (s_idx) .. seed (s_idx) + seed (s_idx + 1) - 1 then
            --  Ok, x_up (now, a seed number) is in the considered seed range.
            --  2) We follow x_down down to category "location",
            --     like for part 1.
            Walk_down_Maps (x_down, r);
            --  x_down is now a location number.
            lowest_2 := Min (lowest_2, x_down);
            exit;
          end if;
        end if;
      end loop;
    end Test;

  begin
    --  The case with the left bound of seed ranges is already
    --  treated by Lowest_Location_Part_1.
    for r in Relation loop
      for li in 1 .. map (r).top loop
        --  Test left bound of explicit range "li" for category "r":
        --  Note: this test is sufficient to find the solution
        --  for data aoc_2023_05.txt, but is certainly due to luck.
        Test (map (r).list (li).source_start, r);
        --  Test left bound of the implicit range that is right
        --  after the explicit one:
        Test (map (r).list (li).source_start + map (r).list (li).length, r);
      end loop;
      --  Test the remaining implicit range, which starts with 0.
      Test (0, r);
    end loop;
    r (part_2) := lowest_2;
  end Lowest_Location_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin

  Read_Data;
  Lowest_Location_Part_1;
  Lowest_Location_Part_2;

  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1))) or
       r (part_2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & r (part_1)'Image);
    Put_Line (+"Part 2:" & r (part_2)'Image);
    --  Part 1: validated by AoC: 261668924
    --  Part 2: validated by AoC: 24261545
  end if;

end AoC_2023_05;
