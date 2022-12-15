--  Solution to Advent of Code 2022, Day 15
-------------------------------------------
--  Beacon Exclusion Zone
--
--  https://adventofcode.com/2022/day/15
--  Copy of questions in: aoc_2022_15_questions.txt

with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

--  Interfaces is needed for compiling on both
--  HAC and GNAT (64-bit integer: Integer_64):
with Interfaces;

procedure AoC_2022_15 is
  use AoC_Toolbox, HAT, Interfaces;

  lowest, highest : Point;

  procedure Adapt_Lowest_Value_Point (using : Point) is
  begin
    lowest.x := HAT.Min (lowest.x, using.x);
    lowest.y := HAT.Min (lowest.y, using.y);
  end Adapt_Lowest_Value_Point;

  procedure Adapt_Highest_Value_Point (using : Point) is
  begin
    highest.x := HAT.Max (highest.x, using.x);
    highest.y := HAT.Max (highest.y, using.y);
  end Adapt_Highest_Value_Point;

  type Observation is record
    sensor, beacon : Point;
    d : Natural;
  end record;

  type Storage is array (1 .. 100) of Observation;

  top : Natural;
  k   : Storage;

  --  If (x, y) is within topological disc (which is a square,
  --  geometrically, due to the distance used, L1) around any
  --  sensor delimited by the distance to the next beacon, then
  --  it is beacon-free [2] unless it is the point where
  --  the beacon is [1].
  --  NB: the puzzle seems to politely exclude *two* beacons
  --  at the same distance of a sensor...
  --
  function Beacon_Free (x, y : Integer) return Boolean is
    p : Point;
  begin
    p.x := x;
    p.y := y;
    for i in 1 .. top loop
      if k (i).beacon.x = x and then k (i).beacon.y = y then
        return False;  --  [1]
      end if;
      if Dist_L1 (p, k (i).sensor) <= k (i).d then
        return True;   --  [2]
      end if;
    end loop;
    return False;
  end Beacon_Free;

  --  Count for x in a given range, and a given y,
  --  the number of beacon-free points.
  function Scan_for_Beacon_Free_Range (mx, nx, y : Integer) return Natural is
    t : Natural := 0;
  begin
    for x in mx .. nx loop
      if Beacon_Free (x, y) then
        t := t + 1;
      end if;
    end loop;
    return t;
  end Scan_for_Beacon_Free_Range;

  --  Count for "any" value of x, and a given y,
  --  the number of beacon-free points.
  function Scan_for_Beacon_Free (y : Integer) return Natural is
  begin
    return
      Scan_for_Beacon_Free_Range
        (lowest.x  - (highest.x - lowest.x),
         highest.x + (highest.x - lowest.x), y);
  end Scan_for_Beacon_Free;

  --  name : constant VString := +"mini.txt";
  --  y_p1 : constant := 10;
  --  lim_p2 : constant := 20;

  name : constant VString := +"aoc_2022_15.txt";
  y_p1 : constant := 2_000_000;
  lim_p2 : constant := 4_000_000;

  tuning_frequency : Integer_64;

  procedure Search_Undetected_Beacon is
    sensor_i : Point;
    distance_i : Integer;
    procedure Check (x, y : Integer) is
    begin
      if x in 0 .. lim_p2
        and then y in 0 .. lim_p2
        and then not Beacon_Free (x, y)
      then
        tuning_frequency := Integer_64 (x) * 4_000_000 + Integer_64 (y);
      end if;
    end;
  begin
    tuning_frequency := 0;
    --
    --  Since there is only one possible point where the beacon is,
    --  it is surrounded by beacon-free zones, which are the topological
    --  discs (geometrically, squares with diagonal edges) around a sensor.
    --  So it is sufficient to search the point right outside the squares.
    --
    --  Idea from Maxim Reznik,
    --  https://forum.ada-lang.io/t/2022-day-15-beacon-exclusion-zone/254/2
    --
    --  The brute force method would check more than 16 trillion points!
    --
    for i in 1 .. top loop
      sensor_i   := k (i).sensor;
      distance_i := k (i).d;
      for dx in 0 .. distance_i loop
        --  Check one unit above the top-right side
        --  of the square around sensor_i #i.
        Check (sensor_i.x + dx, sensor_i.y + (distance_i - dx) + 1);
        --  NB: we should scan the other sides for the special
        --      case where the searched point is on the edge of
        --      the given search area, but in the example and our
        --      data, the point is inside the area, so it is surrounded
        --      by four squares with one kind edge each touching the point.
        --
        --  12222233
        --  11222333
        --  1112*433   <--- the point *. The check above will detect it for
        --  11114443                     square #1.
        --  11144444
        exit when tuning_frequency > 0;
      end loop;
      exit when tuning_frequency > 0;
    end loop;
  end Search_Undetected_Beacon;

  s1 : String (1 .. 12) := "Sensor at x=";
  s2 : String (1 ..  4) := ", y=";
  s3 : String (1 .. 25) := ": closest beacon is at x=";
  s4 : String (1 ..  4) := ", y=";
  f : File_Type;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer_64;

begin
  top := 0;
  lowest.x := Integer'Last;
  lowest.y := Integer'Last;
  highest.x := Integer'First;
  highest.y := Integer'First;
  Open (f, name);
Read_Data :
  while not End_Of_File (f) loop
    top := top + 1;
    Get (f, s1);
    Get (f, k (top).sensor.x);
    Get (f, s2);
    Get (f, k (top).sensor.y);
    Get (f, s3);
    Get (f, k (top).beacon.x);
    Get (f, s4);
    Get (f, k (top).beacon.y);
    k (top).d := Dist_L1 (k (top).beacon, k (top).sensor);
    Adapt_Lowest_Value_Point (k (top).beacon);
    Adapt_Highest_Value_Point (k (top).beacon);
    Adapt_Lowest_Value_Point (k (top).sensor);
    Adapt_Highest_Value_Point (k (top).sensor);
  end loop Read_Data;
  Close (f);

  Search_Undetected_Beacon;

  r (1) := Integer_64 (Scan_for_Beacon_Free (y_p1));
  r (2) := tuning_frequency;

  Put_Line (+"Done in: " & (Clock - T0) & " seconds");
  Put_Line (+"Part 1 answer:" & Integer_64'Image (r (1)));
  Put_Line (+"Part 2 answer:" & Integer_64'Image (r (2)));
  --  Part 1: validated by AoC: 5240818
  --  Part 2: validated by AoC: 13213086906101
end AoC_2022_15;
