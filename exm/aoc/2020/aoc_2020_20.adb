--  Solution to Advent of Code 2020, Day 20
-------------------------------------------
--  Jurassic Jigsaw
--
--  https://adventofcode.com/2020/day/20
--
--  Part 1 only
--
with HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2020_20 is
  use HAT, Interfaces;
  size : constant := 144;
  name : constant VString := +"aoc_2020_20.txt";

  type Side is (up, down, swup, swdown, left, right, swleft, swright);

  subtype Tile_Range is Positive range 1 .. size;
  label : array (Tile_Range) of Natural;
  side_code : array (Side, Tile_Range) of Natural;

  --
  subtype Edge_Value_Range is Natural range 0 .. 1023;
  edge_count : array (Edge_Value_Range) of Natural;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  --
  procedure Get_Nat (f : in out File_Type; n : out Natural) is
    c : Character;
  begin
    n := 0;
    Get (f, c);
    while c = ' ' loop  --  Skip heading blanks.
      Get (f, c);
    end loop;
    loop
      exit when c < '0' or c > '9';
      n := n * 10 + Ord (c) - Ord ('0');
      Get (f, c);
    end loop;
  end Get_Nat;
  --
  procedure Read_Data is
    t_s : String (1 .. 5);  --  "Tile "
    f : File_Type;
    c : Character;
    bit : Integer;
  begin
    Open (f, name);
    for n in Tile_Range loop
      Get (f, t_s);
      Get_Nat (f, label (n));
      for s in Side loop
        side_code (s, n) := 0;
      end loop;
      for i in 0 .. 9 loop
        for j in 0 .. 9 loop
          Get (f, c);
          if c = '#' then
            bit := 1;
          else
            bit := 0;
          end if;
          if i = 0 then  --  top-down (0 = top)
            side_code (up, n)   := side_code (up, n)   + bit * (2 ** j);
            side_code (swup, n) := side_code (swup, n) + bit * (2 ** (9 - j));
          end if;
          if i = 9 then
            side_code (down, n)   := side_code (down, n)   + bit * (2 ** j);
            side_code (swdown, n) := side_code (swdown, n) + bit * (2 ** (9 - j));
          end if;
          if j = 0 then
            side_code (left, n)   := side_code (left, n)   + bit * (2 ** i);
            side_code (swleft, n) := side_code (swleft, n) + bit * (2 ** (9 - i));
          end if;
          if j = 9 then
            side_code (right, n)   := side_code (right, n)   + bit * (2 ** i);
            side_code (swright, n) := side_code (swright, n) + bit * (2 ** (9 - i));
          end if;
        end loop;
      end loop;
    end loop;
    Close (f);
  end Read_Data;
  --
  procedure Count_Edges is
    procedure Increment (value : Natural) is
    begin
      edge_count (value) := edge_count (value) + 1;
    end Increment;
  begin
    for i in Edge_Value_Range loop
      edge_count (i) := 0;
    end loop;
    for m in Tile_Range loop
      for s in Side loop
        Increment (side_code (s, m));
      end loop;
    end loop;
    for i in Edge_Value_Range loop
      if edge_count (i) > 2 then
        Put_Line ("Though puzzle case (luckily, our data isn't)");
        Put_Line (+"" & i & "  : " & edge_count (i));
      end if;
    end loop;
  end Count_Edges;
  --
  function Is_Corner (n : Positive) return Boolean is
  begin
    return
      (edge_count (side_code (left, n))  = 1 and edge_count (side_code (up, n))   = 1) or
      (edge_count (side_code (right, n)) = 1 and edge_count (side_code (up, n))   = 1) or
      (edge_count (side_code (left, n))  = 1 and edge_count (side_code (down, n)) = 1) or
      (edge_count (side_code (right, n)) = 1 and edge_count (side_code (down, n)) = 1);
  end Is_Corner;
  --
  function Spot_Corners return Integer_64 is
    prod : Integer_64 := 1;
  begin
    for n in Tile_Range loop
      if Is_Corner (n) then
        prod := prod * Integer_64 (label (n));
      end if;
    end loop;
    return prod;
  end Spot_Corners;
  --
begin
  Read_Data;
  Count_Edges;
  if compiler_test_mode then
    if Spot_Corners /= Integer_64'Value (To_String (Argument (1))) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line ("Product of corner labels:" & Integer_64'Image (Spot_Corners));
  end if;
  --  20899048083289 (example)
  --  83775126454273 (input)
end AoC_2020_20;
