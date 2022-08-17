--  Solution to Advent of Code 2021, Day 20
-------------------------------------------
--  Trench Map
--
--  A Game-of-life puzzle!
--
--  https://adventofcode.com/2021/day/20
--  Copy of questions in: aoc_2021_20_questions.txt
--
--  Note: this programs takes very long on HAC!
--
--  HAC 0.098 "nice to have"'s detected in this exercise:
--
--    *     Detect an expression as a static (compile-time-known) value
--            ->  Good for arithmetic in number declaration
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_20 is
  use HAT;

  iter_max : constant :=   50;
  margin   : constant :=  101;  --  "full Ada": iter_max * 2 + 1
  map_min  : constant := -101;  --  "full Ada": -margin

  --  input : constant VString := +"mini.txt"; data_max : constant :=  4;
  input : constant VString := +"aoc_2021_20.txt"; data_max : constant :=  99;
  map_max  : constant :=  200;  --  "full Ada": data_max + margin

  iea_max : constant := 511;
  subtype Bit is Integer range 0 .. 1;
  iea : array (0 .. iea_max) of Bit;
  type Map_Type is array (map_min .. map_max, map_min .. map_max) of Bit;
  map : array (0 .. 1) of Map_Type;
  cur_map : Natural := 0;

  procedure Read_Data is
    --
    c : Character;
    s : VString;
    f : File_Type;
  begin
    Open (f, input);
    Get_Line (f, s);
    for i in 0 .. iea_max loop
      if Element (s, i + 1) = '#' then
        iea (i) := 1;
      else
        iea (i) := 0;
      end if;
    end loop;
    Skip_Line (f);
    for y in map_min .. map_max loop
      for x in map_min .. map_max loop
        map (0)(x, y) := 0;
        map (1)(x, y) := 0;
        if x in 0 .. data_max and then y in 0 .. data_max then
          Get (f, c);
          if c = '#' then
            map (cur_map)(x, y) := 1;
          end if;
        end if;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Show_Map (extension : Natural) is
  begin
    for y in -extension .. data_max + extension loop
      for x in -extension .. data_max + extension loop
        if map (cur_map)(x, y) = 1 then
          Put ('#');
        else
          Put ('.');
        end if;
      end loop;
      New_Line;
    end loop;
    New_Line;
  end Show_Map;

  procedure Enhance is
    code : Natural;
    new_map : constant Natural := 1 - cur_map;
  begin
    for y in map_min + 1 .. map_max - 1 loop
      for x in map_min + 1 .. map_max - 1 loop
        code := 0;
        for yy in -1 .. 1 loop
          for xx in -1 .. 1 loop
            code := code * 2 + map (cur_map)(x + xx, y + yy);
          end loop;
        end loop;
        map (new_map)(x, y) := iea (code);
      end loop;
    end loop;
    cur_map := new_map;
  end Enhance;

  function Count (extension : Natural)  return Natural is
    total : Natural := 0;
  begin
    for y in -extension .. data_max + extension loop
      for x in -extension .. data_max + extension loop
        total := total + map (cur_map)(x, y);
      end loop;
    end loop;
    return total;
  end Count;

  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;
  T0 : constant Time := Clock;
begin
  Read_Data;
  if verbose then Show_Map (0); end if;
  for iteration in 1 .. iter_max loop
    Enhance;
    case iteration is
      when      2 => r (1) := Count (2);
      when     50 => r (2) := Count (50);
      when others => null;
    end case;
    if verbose then
      Show_Map (iteration);
    end if;
  end loop;
  if verbose then
    Show_Map (iter_max);
  end if;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: pixels after 2 iterations : " & r (1));
    Put_Line (+"Part 2: pixels after 50 iterations: " & r (2));
    --  Part 1: validated by AoC: 5306
    --  Part 2: validated by AoC: 17497
  end if;
end AoC_2021_20;
