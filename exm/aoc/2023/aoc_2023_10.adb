--  Solution to Advent of Code 2023, Day 10
-------------------------------------------
--  Pipe Maze
--
--  https://adventofcode.com/2023/day/10
--  Copy of questions in: aoc_2023_10_questions.txt
--

--  The files aoc_toolbox.ad* are located in the upper directory (..)
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_10 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 20; nx3 : constant := 60;
  input_name : constant VString := +"aoc_2023_10"; n : constant := 140; nx3 : constant := 420;

  map : array (1 .. n, 1 .. n) of Character;
  si, sj  : Positive;

  procedure Read_Data is
    f : File_Type;
    c : Character;
  begin
    Open (f, input_name & ".txt");
    for i in 1 .. n loop
      for j in 1 .. n loop
        Get (f, c);
        if c = 'S' then
          si := i;
          sj := j;
        end if;
        map (i, j) := c;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer;

  type State is (clean, path, outside);

  visited    : array (1 .. n,   1 .. n)   of State;
  visited_x3 : array (1 .. nx3, 1 .. nx3) of State;

  path_length : Natural := 0;

  procedure Do_Part_1 is
    i, j : Positive;
    iii, jjj : Natural;
    i3, j3 : Integer;
    p : Character;
    procedure Go is
    begin
      i := iii;
      j := jjj;
      visited (i, j) := path;
      path_length := path_length + 1;
    end Go;
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop
        visited (i, j) := clean;
      end loop;
    end loop;
    for i in 1 .. nx3 loop
      for j in 1 .. nx3 loop
        visited_x3 (i, j) := clean;
      end loop;
    end loop;
    i := si;
    j := sj;
    p := map (i, j);
    main :
    loop
      scan :
      for ii in -1 .. 1 loop
        for jj in -1 .. 1 loop
          if abs (ii) + abs (jj) = 1 then
            iii := i + ii;
            jjj := j + jj;
            --  Try next move on iii, jjj
            i3 := 1 + (iii - 1) * 3;
            j3 := 1 + (jjj - 1) * 3;
            if iii in 1 .. n
               and then jjj in 1 .. n
               and then visited (iii, jjj) = clean
            then
              case map (iii, jjj) is
                when '|' =>
                  if        (ii =  1 and then (p = 'S' or else p = '|' or else p = 'F' or else p = '7'))
                    or else (ii = -1 and then (p = 'S' or else p = '|' or else p = 'L' or else p = 'J'))
                  then
                    Go;
                    --  Draw on 3x3 square. Here we have:
                    --
                    --    .#.
                    --    .#.
                    --    .#.
                    --
                    visited_x3 (i3,     j3 + 1) := path;
                    visited_x3 (i3 + 1, j3 + 1) := path;
                    visited_x3 (i3 + 2, j3 + 1) := path;
                    exit scan;
                  end if;
                when '-' =>
                  if        (jj =  1 and then (p = 'S' or else p = '-' or else p = 'L' or else p = 'F'))
                    or else (jj = -1 and then (p = 'S' or else p = '-' or else p = '7' or else p = 'J'))
                  then
                    Go;
                    --  Draw on 3x3 square
                    visited_x3 (i3 + 1, j3)     := path;
                    visited_x3 (i3 + 1, j3 + 1) := path;
                    visited_x3 (i3 + 1, j3 + 2) := path;
                    exit scan;
                  end if;
                when 'L' =>
                  if        (ii =  1 and then (p = 'S' or else p = '|' or else p = 'F' or else p = '7'))
                    or else (jj = -1 and then (p = 'S' or else p = '-' or else p = '7' or else p = 'J'))
                  then
                    Go;
                    --  Draw on 3x3 square. Here we have:
                    --
                    --    .#.
                    --    .##
                    --    ...
                    --
                    visited_x3 (i3,     j3 + 1) := path;
                    visited_x3 (i3 + 1, j3 + 1) := path;
                    visited_x3 (i3 + 1, j3 + 2) := path;
                    exit scan;
                  end if;
                when 'J' =>
                  if        (ii =  1 and then (p = 'S' or else p = '|' or else p = 'F' or else p = '7'))
                    or else (jj =  1 and then (p = 'S' or else p = '-' or else p = 'L' or else p = 'F'))
                  then
                    Go;
                    --  Draw on 3x3 square
                    visited_x3 (i3,     j3 + 1) := path;
                    visited_x3 (i3 + 1, j3)     := path;
                    visited_x3 (i3 + 1, j3 + 1) := path;
                    exit scan;
                  end if;
                when '7' =>
                  if        (ii = -1 and then (p = 'S' or else p = '|' or else p = 'L' or else p = 'J'))
                    or else (jj =  1 and then (p = 'S' or else p = '-' or else p = 'L' or else p = 'F'))
                  then
                    Go;
                    --  Draw on 3x3 square
                    visited_x3 (i3 + 1, j3)     := path;
                    visited_x3 (i3 + 1, j3 + 1) := path;
                    visited_x3 (i3 + 2, j3 + 1) := path;
                    exit scan;
                  end if;
                when 'F' =>
                  if        (ii = -1 and then (p = 'S' or else p = '|' or else p = 'L' or else p = 'J'))
                    or else (jj = -1 and then (p = 'S' or else p = '-' or else p = '7' or else p = 'J'))
                  then
                    Go;
                    --  Draw on 3x3 square
                    visited_x3 (i3 + 1, j3 + 1) := path;
                    visited_x3 (i3 + 1, j3 + 2) := path;
                    visited_x3 (i3 + 2, j3 + 1) := path;
                    exit scan;
                  end if;
                when 'S' =>
                  if        (ii = -1 and then (p = '|' or else p = 'L' or else p = 'J'))
                    or else (jj = -1 and then (p = '-' or else p = '7' or else p = 'J'))
                    or else (ii =  1 and then (p = '|' or else p = 'F' or else p = '7'))
                    or else (jj =  1 and then (p = '-' or else p = 'L' or else p = 'F'))
                  then
                    if path_length > 1 then  --  Avoid immediate return to Start.
                      --  put("back!");
                      Go;
                      --  Draw a cross on 3x3 square
                      visited_x3 (i3,     j3 + 1) := path;
                      visited_x3 (i3 + 1, j3)     := path;
                      visited_x3 (i3 + 1, j3 + 1) := path;
                      visited_x3 (i3 + 1, j3 + 2) := path;
                      visited_x3 (i3 + 2, j3 + 1) := path;
                      exit scan;
                    end if;
                  end if;
                when others =>
                  null;
              end case;
            end if;
          end if;
        end loop;
      end loop scan;
      p := map (i, j);
      exit main when p = 'S';
    end loop main;
    r (part_1) := path_length / 2;
  end Do_Part_1;

  verbose : constant Boolean := False;

  --  PPM picture output, adapted from AoC_2022_12.
  --
  procedure Dump_PPM is
    d : File_Type;
    i, j : Integer;
  begin
    Create (d, input_name & ".ppm");
    Put (d, "P6" & Chr (10));
    Put (d, nx3); Put (d, ' ');
    Put (d, nx3); Put (d, Chr (10));
    Put (d, "255" & Chr (10));
    for i3 in 1 .. nx3 loop
      for j3 in 1 .. nx3 loop
        i := (i3 - 1) / 3 + 1;
        j := (j3 - 1) / 3 + 1;
        case visited_x3 (i3, j3) is
          when clean =>
            if visited (i, j) = clean then
              --  "inside" tile
              Put (d, Chr (230));
              Put (d, Chr (255));
              Put (d, Chr (110));
            else
              --  partly "inside" tile
              Put (d, Chr (160));
              Put (d, Chr (240));
              Put (d, Chr (190));
            end if;
          when path =>
            Put (d, Chr (0));
            Put (d, Chr (0));
            Put (d, Chr (0));
          when outside =>
            if visited (i, j) = outside then
              --  "outside" tile
              Put (d, Chr (255));
              Put (d, Chr (255));
              Put (d, Chr (255));
            else
              --  partly "outside" tile
              Put (d, Chr (220));
              Put (d, Chr (220));
              Put (d, Chr (220));
            end if;
        end case;
      end loop;
    end loop;
    Close (d);
  end Dump_PPM;

  procedure Do_Part_2 is
    procedure Flood_Fill (i, j : Integer) is  --  Taken from aoc_2022_18.
    begin
      if i in 1 .. nx3
        and then j in 1 .. nx3
        and then visited_x3 (i, j) = clean
      then
        visited_x3 (i, j) := outside;
        Flood_Fill (i - 1, j);
        Flood_Fill (i + 1, j);
        Flood_Fill (i, j - 1);
        Flood_Fill (i, j + 1);
      end if;
    end Flood_Fill;
    c : Natural := 0;
    i3, j3, sq : Natural;
  begin
    --  Flood fill the outside part, starting
    --  from the (1, 3) point of the detailed map.
    --  The big path never touches the detailed map's border
    --  (otherwise, the path would not be closed).
    --  Consequently, the set outside the path is connected
    --  and we are fine starting with the (1, 3) point only.
    --  Why not (1, 1)? It is to handle the case where the
    --  start point (S), drawn as a cross, is in the top left corner.
    --
    Flood_Fill (1, 3);

    for i in 1 .. n loop
      for j in 1 .. n loop
        sq := 0;
        i3 := 1 + (i - 1) * 3;
        j3 := 1 + (j - 1) * 3;
        for ii in 0 .. 2 loop
          for jj in 0 .. 2 loop
            if visited_x3 (i3 + ii, j3 + jj) = outside then
              sq := sq + 1;
            end if;
          end loop;
        end loop;
        if sq = 9 then
          --  The entire 3x3 square is marked as "outside".
          --  Then it is neither an inside tile, nor a tile with
          --  the giant loop on it.
          --  In the latter case, we have, for instance for a 'L',
          --  only 8 squares set as visited:
          --
          --      OpI
          --      Opp
          --      OOO
          --
          --  O = outside, set by Flood_Fill
          --  p = path of the giant loop, set by part 1
          --  I = inside; not set.
          --
          c := c + 1;
          visited (i, j) := outside;  --  This is just for the visualisation
        end if;
      end loop;
    end loop;
    --  Inside = surface - path's length - outside.
    r (part_2) := n * n - path_length - c;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  if verbose then
    Dump_PPM;
  end if;
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 6897
    --  Part 2: validated by AoC: 367
  end if;
end AoC_2023_10;
