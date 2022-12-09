--  Solution to Advent of Code 2022, Day 9
------------------------------------------
--  Rope Bridge
--
--  https://adventofcode.com/2022/day/9
--  Copy of questions in: aoc_2022_09_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_09 is
  use HAT;

  verbose : constant Boolean := True;
  T0 : constant Time := Clock;
  res : array (1 .. 2) of Integer;

  c : Character;
  total, steps : Integer;
  f : File_Type;

  m : constant := 1000;
  map : array (1 .. m, 1 .. m) of Boolean;

  procedure Show is
  begin
    for y in reverse 1 .. m loop
      for x in 1 .. m loop
        if map (x, y) then
        Put ('#');
        else
        Put (' ');
        end if;
      end loop;
      New_Line;
    end loop;
  end;

  type Movement is (L, R, U, D);

  move : Movement;

  type Pos is record
    x, y : Natural;
  end record;

  function Dist (a, b : Pos) return Natural is
  begin
    return
      Max (abs (a.x - b.x), abs (a.y - b.y));
  end;

  H, old_H : Pos;

  type Big_Rope is array (1 .. 9) of Pos;

  T, old_T : Big_Rope;

  len : Positive;

  type Follow_Method is (meth_1, meth_2);

  meth : constant Follow_Method := meth_2;

  procedure Follow_2 (a : Pos; b : in out Pos) is
    dx, dy : Integer := 0;
  begin
    if a.x = b.x and then abs (a.y - b.y) > 1 then
      if a.y > b.y then dy := 1; else dy := -1; end if;
    elsif a.y = b.y and then abs (a.x - b.x) > 1 then
      if a.x > b.x then dx := 1; else dx := -1; end if;
    elsif abs (a.y - b.y) + abs (a.x - b.x) > 2 then
      if a.x > b.x then dx := 1; else dx := -1; end if;
      if a.y > b.y then dy := 1; else dy := -1; end if;
    end if;
    b.x := b.x + dx;
    b.y := b.y + dy;
  end;

begin

Parts :
  for part in 1 .. 2 loop
    H.x := m / 2;
    H.y := m / 2;
    for i in T'Range loop
      T (i) := H;
    end loop;
    for x in 1 .. m loop
      for y in 1 .. m loop
        map (x, y) := False;
      end loop;
    end loop;
    map (H.x, H.y) := True;
    case part is
      when 1 => len := 1;
      when 2 => len := 9;
    end case;

    Open (f, "aoc_2022_09.txt");
  Read_Data :
    while not End_Of_File (f) loop
      Get (f, c);
      Get (f, steps);
      move := Movement'Value (c & "");
      for count in 1 .. steps loop
        old_H := H;
        case move is
          when L => H.x := H.x - 1;
          when R => H.x := H.x + 1;
          when D => H.y := H.y - 1;
          when U => H.y := H.y + 1;
        end case;
        if Dist (H, T (1)) < 2 then
          null;
        else
          T (1) := old_H;
        end if;
        for i in 2 .. len loop
          old_T := T;
          case meth is
            when meth_1 =>
              if Dist (T (i - 1), T (i)) < 2 then
                null;
              else
                T (i) := old_T (i - 1);
              end if;
            when meth_2 =>
              Follow_2 (T (i - 1), T (i));
          end case;
        end loop;
        map (T (len).x, T (len).y) := True;
      end loop;
    end loop Read_Data;
    Close (f);

    if verbose and then m < 100 then
      Show;
    end if;

    total := 0;
    for x in 1 .. m loop
      for y in 1 .. m loop
        if map (x, y) then total := total + 1; end if;
      end loop;
    end loop;

    res (part) := total;
  end loop Parts;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if res (1) /= Integer'Value (To_String (Argument (1))) or
       res (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: visited points of tail . . . . . : " & res (1));
    Put_Line (+"Part 2: visited points of tail of rope . : " & res (2));
    --  Part 1: validated by AoC: 6314
    --  Part 2: validated by AoC: 2504
  end if;
end AoC_2022_09;
