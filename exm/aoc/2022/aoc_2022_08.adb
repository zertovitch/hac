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

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;
  c : Character;
  f : File_Type;

  m : constant := 99;
  sc, map : array (1 .. m, 1 .. m) of Natural;
  new_tree : array (1 .. m, 1 .. m) of Boolean;
  h, score, visible_trees : Natural;

begin
  Open (f, "aoc_2022_08.txt");
Read_Data :
  for i in 1 .. m loop
    for j in 1 .. m loop
      Get (f, c);
      map (i, j) := Ord (c) - Ord ('0');
      sc (i, j) := 1;
      new_tree (i, j) := True;
    end loop;
  end loop Read_Data;
  Close (f);

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

  --  `visible_trees` holds now the number of visible
  --  trees that are inside the forest.
  --  The trees on the edge are all visible.

  r (1) := 2 * m + 2 * (m - 2) + visible_trees;

  ------------------------------------
  --  Part 2                        --
  --  Compute highest scenic score  --
  ------------------------------------

  for i in 1 .. m loop
    for j in 1 .. m loop
      score := 0;
      for jj in reverse 1 .. j - 1 loop
        score := score + 1;
        exit when map (i, jj) >= map (i, j);
      end loop;
      sc (i, j) := sc (i, j) * score;
      score := 0;
      for jj in j + 1 .. m loop
        score := score + 1;
        exit when map (i, jj) >= map (i, j);
      end loop;
      sc (i, j) := sc (i, j) * score;
      score := 0;
      for ii in reverse 1 .. i - 1 loop
        score := score + 1;
        exit when map (ii, j) >= map (i, j);
      end loop;
      sc (i, j) := sc (i, j) * score;
      score := 0;
      for ii in i + 1 .. m loop
        score := score + 1;
        exit when map (ii, j) >= map (i, j);
      end loop;
      sc (i, j) := sc (i, j) * score;
    end loop;
  end loop;
  --  Compute highest scenic score:
  score := 0;
  for i in 2 .. m - 1 loop
    for j in 2 .. m - 1 loop
      score :=  Max (score, sc (i, j));
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
