--  Solution to Advent of Code 2022, Day 12
-------------------------------------------
--  Hill Climbing Algorithm
--
--  https://adventofcode.com/2022/day/12
--  Copy of questions in: aoc_2022_12_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_12 is

  type Point is record
    x, y : Integer;
  end record;

  start, finish, n : Point;

  inf : constant Natural := Integer'Last / 4;

  nx_max : constant := 200;
  ny_max : constant := 100;

  subtype Range_x is Integer range 1 .. nx_max;
  subtype Range_y is Integer range 1 .. ny_max;

  map   : array (Range_x, Range_y) of Character;
  best  : array (Range_x, Range_y) of Natural;

  use HAT;

  procedure Data_Acquisition is
    c : Character;
    f : File_Type;
    x, y : Natural := 0;
  begin
    Open (f, "aoc_2022_12.txt");
  Read_Data :
    while not End_Of_File (f) loop
      y := y + 1;
      x := 0;
      Get (f, c);
      loop
        x := x + 1;
        case c is
          when 'S' =>
            start.x := x;
            start.y := y;
            map (x, y) := 'a';
          when 'E' =>
            finish.x := x;
            finish.y := y;
            map (x, y) := 'z';
          when others =>
            map (x, y) := c;
        end case;
        exit when End_Of_Line (f);
        Get (f, c);
      end loop;
    end loop Read_Data;
    n.x := x;
    n.y := y;
    Close (f);
  end Data_Acquisition;

  --  Dijkstra shortest path algorithm.
  --  Code adapted from AoC_2021_15.
  --
  --  The following definitions belong to the Dijkstra algorithm, but
  --  we keep them less local because of the visualization.

  list_length_max : constant := 7500;
  subtype List_Range is Integer range 1 .. list_length_max;

  type Node is record
    len  : Natural;
    pt   : Point;
    pred : Natural;  --  This is just for displaying the path.
  end record;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  function Dijkstra_Algorithm (start : Point; part : Positive) return Natural is
    --
    --  `Visit` is similar to the `Search_Recursive` above, but not recursive.
    --
    procedure Visit (x, y, len_from, node_from : Integer; c_from : Character) is
      len_to, ins : Integer;
      ok_step : Boolean;
    begin
      if x in 1 .. n.x and then y in 1 .. n.y then
        if part = 1 then
          --  We avoid ascending more than one unit.
          ok_step := map (x, y) <= Succ (c_from);
        else
          --  We avoid descending more than one unit (reverse walk).
          ok_step := map (x, y) >= Pred (c_from);
        end if;
        if ok_step then
          len_to := len_from + 1;
          if len_to < best (x, y) then
            --  Improvement on cell (x, y).
            best (x, y) := len_to;
            --
            --  Insert in a sorted way.
            --
            ins := explored + 1;
            for i in current + 1 .. explored loop
              if len_to < list (i).len then
                ins := i;  --  Insert here.
                --  Optional: remove another node
                --  with the same (x, y) and a larger length.
                exit;
              end if;
            end loop;
            for i in reverse ins .. explored loop
              list (i + 1) := list (i);
            end loop;
            list (ins).len := len_to;
            list (ins).pt.x := x;
            list (ins).pt.y := y;
            list (ins).pred := node_from;
            explored := explored + 1;
          end if;
        end if;
      end if;
    end Visit;
    --
    cur_pt  : Point;
    cur_len : Natural;
    cur_c   : Character;
  begin
    current  := 0;
    explored := 0;

    for x in 1 .. n.x loop
      for y in 1 .. n.y loop
        best (x, y) := inf;
      end loop;
    end loop;

    best (start.x, start.y) := 0;
    cur_pt := start;
    loop
      cur_len := best (cur_pt.x, cur_pt.y);
      cur_c := map (cur_pt.x, cur_pt.y);
      Visit (cur_pt.x - 1, cur_pt.y, cur_len, current, cur_c);
      Visit (cur_pt.x + 1, cur_pt.y, cur_len, current, cur_c);
      Visit (cur_pt.x, cur_pt.y - 1, cur_len, current, cur_c);
      Visit (cur_pt.x, cur_pt.y + 1, cur_len, current, cur_c);
      --
      --  Switch to the next best explored point.
      --
      current := current + 1;
      if current > explored then
        Put_Line ("No way found.");
        return inf;
      end if;
      cur_pt := list (current).pt;
      if part = 1 then
        exit when cur_pt.x = finish.x and then cur_pt.y = finish.y;
      else
        exit when map (cur_pt.x, cur_pt.y) = 'a';
      end if;
    end loop;
    return best (cur_pt.x, cur_pt.y);
  end Dijkstra_Algorithm;

  ---------------
  --  Display  --
  ---------------

  package Display is

    subtype Color_Range is Natural range 0 .. 255;
    type Pixel is record
      R, G, B : Color_Range;
    end record;

    black, red, green, blue, yellow, pink, white : Pixel;

    min_x : Range_x;
    max_x : Range_x;
    min_y : Range_y;
    max_y : Range_y;
    bitmap : array (Range_x, Range_y) of Pixel;

    procedure Init;
    procedure Show_Map;
    procedure Show_Path (path_color : Pixel; part : Positive);
    procedure Show_Ends;
    procedure Dump_PPM (title : VString);

  end Display;

  package body Display is

    procedure Init is
    begin
      Display.min_x := 1;
      Display.max_x := n.x;  --  200;
      Display.min_y := 1;    --  401;
      Display.max_y := n.y;  --  990;
      --
      black.R := 0;
      black.G := 0;
      black.B := 0;
      --
      white.R := 16#ff#;
      white.G := 16#ff#;
      white.B := 16#ff#;
      --
      red   := black;
      red.R := 16#ff#;
      green   := black;
      green.G := 16#ff#;
      blue   := black;
      blue.B := 16#ff#;
      --
      yellow := white;
      yellow.B := 16#00#;
      --
      pink.R := 255;
      pink.G := 174;
      pink.B := 201;
    end;

    procedure Show_Map is
      height, value : Real;
      px : Pixel;
    begin
      --  Display the map on the RGB bitmap:
      for y in min_y .. max_y loop
        for x in min_x .. max_x loop
          height := Real (Ord (map (x, y)) - Ord ('a')) / 25.0;
          value := height ** 0.4;  --  Enhance low values.
          px.R := 10 + Integer  (90.0 * value);
          px.G := 80 + Integer (115.0 * value);
          px.B := 50 + Integer (110.0 * value);
          bitmap (x, y) := px;
        end loop;
      end loop;
    end Show_Map;

    procedure Show_Path (path_color : Pixel; part : Positive) is
      px, path : Pixel;
      i, j : Natural;
      fall : Boolean;
    begin
      --  Display optimal path, back from the end (E).
      i := current;
      while i /= 0 loop
        j := list (i).pred;
        if i = current then
          bitmap (list (i).pt.x, list (i).pt.y) := path_color;
        elsif j /= 0 then
          px := bitmap (list (i).pt.x, list (i).pt.y);
          if part = 1 then
            fall :=
              map (list (i).pt.x, list (i).pt.y) <
              map (list (j).pt.x, list (j).pt.y);
          else
            --  The path is walked in reverse, from the
            --  hill's top to the plains.
            fall :=
              map (list (i).pt.x, list (i).pt.y) >
              map (list (j).pt.x, list (j).pt.y);
          end if;
          if fall then
            --  Show differently the point(s)
            --  where the guy falls.
            path := red;
          else
            path := path_color;
          end if;
          --  Blend current pixel with the color chose for the path:
          px.R := (path.R + 2 * px.R) / 3;
          px.G := (path.G + 2 * px.G) / 3;
          px.B := (path.B + 2 * px.B) / 3;
          bitmap (list (i).pt.x, list (i).pt.y) := px;
        end if;
        i := j;
      end loop;
    end Show_Path;

    procedure Show_Ends is
    begin
      --  Display start (S) and end (E) points.
      bitmap (start.x, start.y) := green;
      bitmap (finish.x, finish.y) := blue;
    end Show_Ends;

    --  PPM output adapted from AoC_2022_09.
    --
    procedure Dump_PPM (title : VString) is
      d : File_Type;
    begin
      Create (d, title & ".ppm");
      Put (d, "P6" & Chr (10));
      Put (d, max_x - min_x + 1); Put (d, ' ');
      Put (d, max_y - min_y + 1); Put (d, Chr (10));
      Put (d, "255" & Chr (10));
      for y in min_y .. max_y loop
        for x in min_x .. max_x loop
          Put (d, Chr (bitmap (x, y).R));
          Put (d, Chr (bitmap (x, y).G));
          Put (d, Chr (bitmap (x, y).B));
        end loop;
      end loop;
      Close (d);
    end Dump_PPM;

  end Display;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := not compiler_test_mode;

begin
  Data_Acquisition;
  if verbose then
    Display.Init;
    Display.Show_Map;
  end if;
  r (1) := Dijkstra_Algorithm (start, 1);
  if verbose then
    Display.Show_Path (Display.yellow, 1);
  end if;
  r (2) := Dijkstra_Algorithm (finish, 2);
  if verbose then
    Display.Show_Path (Display.pink, 2);
    Display.Show_Ends;
    Display.Dump_PPM (+"height_map");
  end if;

  if compiler_test_mode then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: shortest path from S to E . . . .  . . : " & r (1));
    Put_Line (+"Part 2: shortest path from E to the ground . . : " & r (2));
    --  Part 1: validated by AoC: 440
    --  Part 2: validated by AoC: 439
  end if;
end AoC_2022_12;
