--  Solution to Advent of Code 2022, Day 8
------------------------------------------
--  Treetop Tree House
--
--  https://adventofcode.com/2022/day/8
--  Copy of questions in: aoc_2022_08_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_08 is
  use HAT;

  m : constant := 99;
  subtype Tree_Height is Natural range 0 .. 9;
  map : array (1 .. m, 1 .. m) of Tree_Height;

  dump : constant Boolean := False;

  procedure Dump_CSV is
    d : File_Type;
  begin
    Create (d, "forest.csv");
    for i in 1 .. m loop
      for j in 1 .. m loop
        Put (d, map (i, j));
        Put (d, ';');
      end loop;
      New_Line (d);
    end loop;
    Close (d);
  end Dump_CSV;

  --  PPM output. Sources:
  --  https://codeberg.org/rommudoh/aoc2022-Ada/src/branch/main/src/day08.adb
  --  https://rosettacode.org/wiki/Bitmap/Write_a_PPM_file#Ada
  --
  procedure Dump_PPM is
    subtype Color_Range is Natural range 0 .. 255;
    type Pixel is record
      R, G, B : Color_Range;
    end record;
    d : File_Type;
    px : Pixel;
  begin
    Create (d, "forest.ppm");
    Put (d, "P6" & Chr (10));
    Put (d, m); Put (d, ' '); Put (d, m); Put (d, Chr (10));
    Put (d, "255" & Chr (10));
    for i in 1 .. m loop
      for j in 1 .. m loop
        case map (i, j) is
          when 0 .. 1 => px.R := 16#99#; px.G := 16#99#; px.B := 16#ff#;
          when 2 .. 3 => px.R := 16#99#; px.G := 16#33#; px.B := 16#66#;
          when 4 .. 5 => px.R := 16#ff#; px.G := 16#ff#; px.B := 16#cc#;
          when 6 .. 7 => px.R := 16#cc#; px.G := 16#ff#; px.B := 16#ff#;
          when 8 .. 9 => px.R := 16#66#; px.G := 16#00#; px.B := 16#66#;
        end case;
        Put (d, Chr (px.R));
        Put (d, Chr (px.G));
        Put (d, Chr (px.B));
      end loop;
    end loop;
    Close (d);
  end Dump_PPM;

  new_tree : array (1 .. m, 1 .. m) of Boolean;
  scenic : array (1 .. m, 1 .. m) of Natural;
  h : Tree_Height;
  score, visible_trees : Natural;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;
  c : Character;
  f : File_Type;

begin
  Open (f, "aoc_2022_08.txt");
Read_Data :
  for i in 1 .. m loop
    for j in 1 .. m loop
      Get (f, c);
      map (i, j) := Ord (c) - Ord ('0');
      scenic (i, j) := 1;
      new_tree (i, j) := True;
    end loop;
  end loop Read_Data;
  Close (f);

  if dump then
    --  Just for plotting the forest :-)
    Dump_CSV;
    Dump_PPM;
  end if;

  ---------------------------
  --  Part 1               --
  --  Count visible trees  --
  ---------------------------

  visible_trees := 0;

Horizontal_Scans :
  for i in 2 .. m - 1 loop

    h := map (i, 1);
  From_Left :
    for j in 2 .. m - 1 loop
      if map (i, j) > h then
        if new_tree (i, j) then
          visible_trees := visible_trees + 1;
          new_tree (i, j) := False;
        end if;
        h := map (i, j);
      end if;
    end loop From_Left;

    h := map (i, m);
  From_Right :
    for j in reverse 2 .. m - 1 loop
      if map (i, j) > h then
        if new_tree (i, j) then
          visible_trees := visible_trees + 1;
          new_tree (i, j) := False;
        end if;
        h := map (i, j);
      end if;
    end loop From_Right;
  end loop Horizontal_Scans;

Vertical_Scans :
  for j in 2 .. m - 1 loop

    h := map (1, j);
  From_Top :
    for i in 2 .. m - 1 loop
      if map (i, j) > h then
        if new_tree (i, j) then
          visible_trees := visible_trees + 1;
          new_tree (i, j) := False;
        end if;
        h := map (i, j);
      end if;
    end loop From_Top;

    h := map (m, j);
  From_Bottom :
    for i in reverse 2 .. m - 1 loop
      if map (i, j) > h then
        if new_tree (i, j) then
          visible_trees := visible_trees + 1;
          new_tree (i, j) := False;
        end if;
        h := map (i, j);
      end if;
    end loop From_Bottom;
  end loop Vertical_Scans;

  --  Variable `visible_trees` holds now the number
  --  of visible trees that are inside the forest.
  --  The trees on the edge are all visible.

  r (1) := 2 * m + 2 * (m - 2) + visible_trees;

  ------------------------------------
  --  Part 2                        --
  --  Compute highest scenic score  --
  ------------------------------------

  for i in 1 .. m loop
    for j in 1 .. m loop
      score := 0;
    To_the_Left :
      for jj in reverse 1 .. j - 1 loop
        score := score + 1;
        exit To_the_Left when map (i, jj) >= map (i, j);
      end loop To_the_Left;
      scenic (i, j) := scenic (i, j) * score;
      score := 0;
    To_the_Right :
      for jj in j + 1 .. m loop
        score := score + 1;
        exit To_the_Right when map (i, jj) >= map (i, j);
      end loop To_the_Right;
      scenic (i, j) := scenic (i, j) * score;
      score := 0;
    To_the_Top :
      for ii in reverse 1 .. i - 1 loop
        score := score + 1;
        exit To_the_Top when map (ii, j) >= map (i, j);
      end loop To_the_Top;
      scenic (i, j) := scenic (i, j) * score;
      score := 0;
    To_the_Bottom :
      for ii in i + 1 .. m loop
        score := score + 1;
        exit To_the_Bottom when map (ii, j) >= map (i, j);
      end loop To_the_Bottom;
      scenic (i, j) := scenic (i, j) * score;
    end loop;
  end loop;
  --  Compute highest scenic score:
  score := 0;
  for i in 2 .. m - 1 loop
    for j in 2 .. m - 1 loop
      score :=  Max (score, scenic (i, j));
    end loop;
  end loop;
  r (2) := score;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: visible trees . . . . . . : " & r (1));
    Put_Line (+"Part 2: highest scenic score  . . : " & r (2));
    --  Part 1: validated by AoC: 1843
    --  Part 2: validated by AoC: 180000
  end if;
end AoC_2022_08;
