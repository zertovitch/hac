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

  mx : constant := 300;
  my : constant := 1000;
  type Column is array (1 .. my) of Boolean;
  empty : Column;
  map : array (1 .. mx) of Column;

  --  PPM output adapted from AoC_2022_08.
  --
  procedure Dump_PPM (title : VString) is
    subtype Color_Range is Natural range 0 .. 255;
    type Pixel is record
      R, G, B : Color_Range;
    end record;
    d : File_Type;
    px : Pixel;
    min_x : constant := 1;
    max_x : constant := mx;  --  200;
    min_y : constant := 1;   --  401;
    max_y : constant := my;  --  990;
  begin
    Create (d, title & ".ppm");
    Put (d, "P6" & Chr (10));
    Put (d, max_x - min_x + 1); Put (d, ' ');
    Put (d, max_y - min_y + 1); Put (d, Chr (10));
    Put (d, "255" & Chr (10));
    for y in reverse min_y .. max_y loop
      for x in min_x .. max_x loop
        case map (x)(y) is
          when False => px.R := 16#ff#; px.G := 16#ff#; px.B := 16#cc#;
          when True  => px.R := 16#66#; px.G := 16#00#; px.B := 16#66#;
        end case;
        Put (d, Chr (px.R));
        Put (d, Chr (px.G));
        Put (d, Chr (px.B));
      end loop;
    end loop;
    Close (d);
  end Dump_PPM;

  type Movement is (L, R, U, D);

  move : Movement;

  type Pos is record
    x, y : Natural;
  end record;

  type Rope_Type is array (0 .. 9) of Pos;

  rope, old_rope : Rope_Type;

  len : Positive;

  type Method_for_following is (use_previous_position, reduce_distance);

  meth : constant Method_for_following := reduce_distance;

  verbose : constant Boolean := False;
  T0 : constant Time := Clock;
  res : array (1 .. 2) of Integer;

  c : Character;
  total, steps : Integer;
  f : File_Type;

  function Dist (a, b : Pos) return Natural is
  begin
    return
      Max (abs (a.x - b.x), abs (a.y - b.y));
  end Dist;

  procedure Follow_Geometrically (head : in Pos; tail : in out Pos) is
    function Sign (I : Integer) return Integer is
    begin
      if I > 0 then
        return 1;
      elsif I < 0 then
        return -1;
      else
        return 0;
      end if;
    end Sign;
  begin
    --  The tail follows the head with a step of maximum 1 in each dimension.
    tail.x := tail.x + Sign (head.x - tail.x);
    tail.y := tail.y + Sign (head.y - tail.y);
  end Follow_Geometrically;

begin
  for y in Column'Range loop
    empty (y) := False;
  end loop;

Parts :
  for part in 1 .. 2 loop
    for i in rope'Range loop
      rope (i).x := mx / 2;
      rope (i).y := my / 2;
    end loop;
    for x in map'Range loop
      map (x) := empty;
    end loop;
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
        old_rope := rope;
        --  Move the head of the rope:
        case move is
          when L => rope (0).x := rope (0).x - 1;
          when R => rope (0).x := rope (0).x + 1;
          when D => rope (0).y := rope (0).y - 1;
          when U => rope (0).y := rope (0).y + 1;
        end case;
        for i in 1 .. len loop
          if Dist (rope (i - 1), rope (i)) > 1 then
            case meth is
              when use_previous_position =>
                rope (i) := old_rope (i - 1);
              when reduce_distance =>
                Follow_Geometrically (rope (i - 1), rope (i));
            end case;
          end if;
        end loop;
        map (rope (len).x)(rope (len).y) := True;
      end loop;
    end loop Read_Data;
    Close (f);

    if verbose then
      Dump_PPM
        (To_Lower
           (+"length_" & Image (len) & '_' &
            Method_for_following'Image (meth)));
    end if;

    total := 0;
    for x in 1 .. mx loop
      for y in 1 .. my loop
        if map (x)(y) then total := total + 1; end if;
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
    Put_Line (+"Method for following: " & To_Lower (+Method_for_following'Image (meth)));
    Put_Line (+"Number of visited points tail for a rope of...");
    Put_Line (+"  (part 1) length 1: " & res (1));
    Put_Line (+"  (part 2) length 9: " & res (2));
    --  Part 1: validated by AoC: 6314
    --  Part 2: validated by AoC: 2504
  end if;
end AoC_2022_09;
