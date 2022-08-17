--  Solution to Advent of Code 2021, Day 15
-------------------------------------------
--  Chiton
--
--  https://adventofcode.com/2021/day/15
--  Copy of questions in: aoc_2021_15_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_15 is
  use HAT;
  sx_max : constant := 500;
  sy_max : constant := 500;
  map : array (1 .. sx_max, 1 .. sy_max) of Natural;
  --  Cumulative minimal risk from starting point to (x, y)
  c_risk : array (1 .. sx_max, 1 .. sy_max) of Natural;
  risk_max : constant Integer := Integer'Last;
  sx, sy : Natural;
  input : constant VString := +"aoc_2021_15.txt";
  --
  procedure Read_Data is
    c : Character;
    f : File_Type;
    x, y : Natural := 0;
  begin
    Open (f, input);
    while not End_Of_File (f) loop
      y := y + 1;
      x := 0;
      Get (f, c);
      loop
        x := x + 1;
        map (x, y) := Ord (c) - Ord ('0');
        c_risk (x, y) := risk_max;
        exit when End_Of_Line (f);
        Get (f, c);
      end loop;
    end loop;
    sx := x;
    sy := y;
    Close (f);
  end Read_Data;
  --
  r : array (1 .. 2) of Integer;
  --
  --  `Visit` explores adjacent cells (recursive algorithm).
  --  Side effect: it writes into `risk`.
  --
  procedure Visit (x, y, risk_from : Integer) is
    risk_to : Integer;
  begin
    if x in 1 .. sx and then y in 1 .. sy then
      risk_to := risk_from + map (x, y);
      if risk_to < c_risk (x, y) then
        --  Ah-ha, we have found a better way, or perhaps
        --  the first way, to (x, y).
        c_risk (x, y) := risk_to;
        --  Since we have a reduced risk (x, y), it's worth
        --  continuing the exploration.
        Visit (x - 1, y, risk_to);
        Visit (x + 1, y, risk_to);
        Visit (x, y - 1, risk_to);
        Visit (x, y + 1, risk_to);
      end if;
    end if;
  end Visit;
  --
  --  Now enters Dijkstra.
  --  How many ready-made sorting containers are currently
  --  available to HAC ? None! I.e., do it yourself!
  --
  procedure Dijkstra (start_x, start_y : Integer) is
    list_length_max : constant := 250_000;  --  sx_max * sy_max;
    type Node is record
      c_risk : Natural;
      x, y   : Positive;
    end record;
    list : array (1 .. list_length_max) of Node;
    current, explored : Natural := 0;  --  0 <= current <= explored
    --
    --  Similar to the `Visit` above, but not recursive.
    --
    procedure Visit (x, y, risk_from : Integer) is
      risk_to, ins : Integer;
    begin
      if x in 1 .. sx and then y in 1 .. sy then
        risk_to := risk_from + map (x, y);
        if risk_to < c_risk (x, y) then
          --  Improvement on cell (x, y).
          --  This happens only once per (x, y) point in this problem - see below.
          c_risk (x, y) := risk_to;
          --
          --  Insert in a sorted way (slooow on HAC for Part 2).
          --
          ins := explored + 1;
          for i in current + 1 .. explored loop
            if risk_to < list (i).c_risk then
              ins := i;  --  Insert here.
              --  NB: we might want to remove another node with the same (x, y) and a
              --  larger risk, but that case never happens in this specific problem!
              --  Reason: the cost for reaching (x, y) is the same from every
              --  side (= map (x, y)), and we reach it from (cur_x, cur_y) which has
              --  the minimum score or (x, y)'s neighbours.
              exit;
            end if;
          end loop;
          for i in reverse ins .. explored loop
            list (i + 1) := list (i);
          end loop;
          list (ins).c_risk := risk_to;
          list (ins).x := x;
          list (ins).y := y;
          explored := explored + 1;
        end if;
      end if;
    end Visit;
    --
    cur_x, cur_y : Positive;
    risk_cur : Natural;
  begin
    c_risk (start_x, start_y) := 0;
    cur_x := start_x;
    cur_y := start_y;
    loop
      risk_cur := c_risk (cur_x, cur_y);
      Visit (cur_x - 1, cur_y, risk_cur);
      Visit (cur_x + 1, cur_y, risk_cur);
      Visit (cur_x, cur_y - 1, risk_cur);
      Visit (cur_x, cur_y + 1, risk_cur);
      --
      --  Switch to the next best explored point.
      --
      current := current + 1;
      cur_x := list (current).x;
      cur_y := list (current).y;
      exit when cur_x = sx and then cur_y = sy;
    end loop;
  end Dijkstra;
  --
  procedure Enlarge_Map is
    xn, yn : Integer;
  begin
    for tile_x in 0 .. 4 loop
      for tile_y in 0 .. 4 loop
        for x in 1 .. sx loop
          xn := x + sx * tile_x;
          for y in 1 .. sy loop
            yn := y + sy * tile_y;
            map (xn, yn) := 1 + (map (x, y) - 1 + tile_x + tile_y) mod 9;
            c_risk (xn, yn) := risk_max;
          end loop;
        end loop;
      end loop;
    end loop;
    sx := sx * 5;
    sy := sy * 5;
  end Enlarge_Map;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  fast : constant Boolean := True;
  T0 : constant Time := Clock;
begin
  Read_Data;
  for part in 1 .. 2 loop
    if part = 2 then
      Enlarge_Map;
    end if;
    if fast then
      Dijkstra (1, 1);
    else
      Visit (1, 1, -map (1, 1));
    end if;
    r (part) := c_risk (sx, sy);
    exit when compiler_test_mode;
    --  ^ We do only part 1 when in test mode.
    --    Parts 1 & 2 take 96 seconds on an i7 9700 with HAC,
    --    and only 0.047 seconds with GNAT.
  end loop;
  --
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: total risk on small map: " & r (1));
    Put_Line (+"Part 2: total risk on large map: " & r (2));
    --  Part 1: validated by AoC: 656
    --  Part 2: validated by AoC: 2979
  end if;
end AoC_2021_15;
