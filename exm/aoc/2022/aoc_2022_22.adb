--  Solution to Advent of Code 2022, Day 22
-------------------------------------------
--  Monkey Map
--
--  https://adventofcode.com/2022/day/22
--  Copy of questions in: aoc_2022_22_questions.txt

with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

procedure AoC_2022_22 is

  use AoC_Toolbox, HAT;

  subtype Range_x is Integer range 1 .. 150;
  subtype Range_y is Integer range 1 .. 200;

  map   : array (Range_x, Range_y) of Character;

  lowest, highest : Point;

  procedure Adapt_Highest_Value_Point (using : Point) is
  begin
    highest.x := Max (highest.x, using.x);
    highest.y := Max (highest.y, using.y);
  end Adapt_Highest_Value_Point;

  procedure Show is
  begin
    for y in lowest.y .. highest.y loop
      for x in lowest.x .. highest.x loop
        Put (map (x, y));
      end loop;
      Put_Line ('|');
    end loop;
  end Show;

  procedure Clear is
  begin
    for y in Range_y loop
      for x in Range_x loop
        map (x, y) := ' ';
      end loop;
    end loop;
  end Clear;

  cube_side : array (Data_Type) of Positive;

  function Locate_Cube_Face (p : Point; data : Data_Type) return Natural is
    q : Point;
    side : constant Positive := cube_side (data);
  begin
    q.x := (p.x - 1) / side;
    q.y := (p.y - 1) / side;

    case data is

      when mini =>
        case q.x is
          when 0 => return 2;
          when 1 => return 3;
          when 2 =>
            case q.y is
              when 0 => return 1;
              when 1 => return 4;
              when 2 => return 5;
              when others => null;
            end case;
          when 3 => return 6;
          when others => null;
        end case;

      when input =>
        case q.x is
          when 0 =>
            case q.y is
              when 2 => return 4;
              when 3 => return 6;
              when others => null;
            end case;
          when 1 =>
            case q.y is
              when 0 => return 1;
              when 1 => return 3;
              when 2 => return 5;
              when others => null;
            end case;
          when 2 =>
            return 2;
          when others => null;
        end case;
    end case;

    Put ("Locate_Cube_Face issue");

    return 0;
  end Locate_Cube_Face;

  subtype Cube_Face_ID is Integer range 1 .. 6;

  procedure Blocks_to_Origin (bo : out Point; face_id : Cube_Face_ID; data : Data_Type) is
  begin
    case data is

      when mini =>
        case face_id is
          when 1 => bo.x := 2; bo.y := 0;
          when 2 => bo.x := 0; bo.y := 1;
          when 3 => bo.x := 1; bo.y := 1;
          when 4 => bo.x := 2; bo.y := 1;
          when 5 => bo.x := 2; bo.y := 2;
          when 6 => bo.x := 3; bo.y := 2;
        end case;

      when input =>
        case face_id is
          when 1 => bo.x := 1; bo.y := 0;
          when 2 => bo.x := 2; bo.y := 0;
          when 3 => bo.x := 1; bo.y := 1;
          when 4 => bo.x := 0; bo.y := 2;
          when 5 => bo.x := 1; bo.y := 2;
          when 6 => bo.x := 0; bo.y := 3;
        end case;

    end case;
  end Blocks_to_Origin;

  procedure Relative_to_Face (p : in out Point; face_id : Cube_Face_ID; data : Data_Type; way : Integer) is
    side_length : constant Positive := cube_side (data);
    bo : Point;
  begin
    Blocks_to_Origin (bo, face_id, data);
    p.x := p.x - way * bo.x * side_length;
    p.y := p.y - way * bo.y * side_length;
  end Relative_to_Face;

  procedure Relative_to_current_Face (p : in out Point; data : Data_Type) is
  begin
    Relative_to_Face (p, Locate_Cube_Face (p, data), data, 1);
  end Relative_to_current_Face;

  verbosity_level : constant Natural := 0;

  procedure Run
    (part     :     Part_Type;
     data     :     Data_Type;
     path     :     VString;
     start    :     Point;
     d, final : out Point)
  is
    dxr, dyr : Real;
    walking_steps : Integer;
    c, co : Character;
    p, left, right, up, down : Point;
    side_length : constant Positive := cube_side (data);

    function Inv (coord : Positive) return Positive is
    begin
      return 1 + side_length - coord;
    end Inv;

    procedure Go is
      new_face : Natural := 0;
      rp, np, nd : Point;

      procedure From_Left (set_new_face : Positive) is
      begin
        new_face := set_new_face;
        np.x := 1;
        nd := right;
      end From_Left;

      procedure From_Right (set_new_face : Positive) is
      begin
        new_face := set_new_face;
        np.x := side_length;
        nd := left;
      end From_Right;

      procedure From_Top (set_new_face : Positive) is
      begin
        new_face := set_new_face;
        np.y := 1;
        nd := down;
      end From_Top;

      procedure From_Bottom (set_new_face : Positive) is
      begin
        new_face := set_new_face;
        np.y := side_length;
        nd := up;
      end From_Bottom;

      procedure Wrap_around_through_Left_Side is
      begin
        for x in reverse Range_x loop
          if map (x, np.y) /= ' ' then
            np.x := x;
            exit;
          end if;
        end loop;
      end Wrap_around_through_Left_Side;

      procedure Wrap_around_through_Right_Side is
      begin
        for x in Range_x loop
          if map (x, np.y) /= ' ' then
            np.x := x;
            exit;
          end if;
        end loop;
      end Wrap_around_through_Right_Side;

      procedure Wrap_around_through_Top_Side is
      begin
        for y in reverse Range_y loop
          if map (np.x, y) /= ' ' then
            np.y := y;
            exit;
          end if;
        end loop;
      end Wrap_around_through_Top_Side;

      procedure Wrap_around_through_Bottom_Side is
      begin
        for y in Range_y loop
          if map (np.x, y) /= ' ' then
            np.y := y;
            exit;
          end if;
        end loop;
      end Wrap_around_through_Bottom_Side;

      procedure Cube_Crawl_through_Left_Side is
      begin
        rp := p;  --  Point relative to current face.
        Relative_to_current_Face (rp, data);
        case data is

          when mini =>
            case Locate_Cube_Face (p, data) is
              when 1 => From_Top    (3); np.x := rp.y;
              when 2 => From_Bottom (6); np.x := Inv (rp.y);
              when 5 => From_Bottom (3); np.x := Inv (rp.y);
              when others =>
                null;
            end case;

          when input =>
            case Locate_Cube_Face (p, data) is
              when 1 => From_Left (4); np.y := Inv (rp.y);
              when 3 => From_Top  (4); np.x := rp.y;
              when 4 => From_Left (1); np.y := Inv (rp.y);
              when 6 => From_Top  (1); np.x := rp.y;
              when others =>
                null;
            end case;
        end case;
        Relative_to_Face (np, new_face, data, -1);
      end Cube_Crawl_through_Left_Side;

      procedure Cube_Crawl_through_Right_Side is
      begin
        rp := p;  --  Point relative to current face.
        Relative_to_current_Face (rp, data);
        case data is

          when mini =>
            case Locate_Cube_Face (p, data) is
              when 1 => From_Right (6);   np.y := Inv (rp.y);
              when 4 => From_Top   (6);   np.x := Inv (rp.y);
              when 6 => From_Right (1);   np.y := Inv (rp.y);
              when others =>
                null;
            end case;

          when input =>
            case Locate_Cube_Face (p, data) is
              when 2 => From_Right  (5); np.y := Inv (rp.y);
              when 3 => From_Bottom (2); np.x := rp.y;
              when 5 => From_Right  (2); np.y := Inv (rp.y);
              when 6 => From_Bottom (5); np.x := rp.y;
              when others =>
                null;
            end case;
        end case;
        Relative_to_Face (np, new_face, data, -1);
      end Cube_Crawl_through_Right_Side;

      procedure Cube_Crawl_through_Top_Side is
      begin
        rp := p;  --  Point relative to current face.
        Relative_to_current_Face (rp, data);
        case data is

          when mini =>
            case Locate_Cube_Face (p, data) is
              when 2 => From_Top   (1); np.x := Inv (rp.x);
              when 3 => From_Left  (1); np.y := rp.x;
              when 1 => From_Top   (2); np.x := Inv (rp.x);
              when 6 => From_Right (4); np.y := Inv (rp.x);
              when others =>
                null;
            end case;

          when input =>
            case Locate_Cube_Face (p, data) is
              when 1 => From_Left   (6); np.y := rp.x;
              when 2 => From_Bottom (6); np.x := rp.x;
              when 4 => From_Left   (3); np.y := rp.x;
              when others =>
                null;
            end case;
        end case;
        Relative_to_Face (np, new_face, data, -1);
      end Cube_Crawl_through_Top_Side;

      procedure Cube_Crawl_through_Bottom_Side is
      begin
        rp := p;  --  Point relative to current face.
        Relative_to_current_Face (rp, data);
        case data is

          when mini =>
            case Locate_Cube_Face (p, data) is
              when 2 => From_Bottom (5); np.x := Inv (rp.x);
              when 3 => From_Left   (5); np.y := Inv (rp.x);
              when 5 => From_Bottom (2); np.x := Inv (rp.x);
              when 6 => From_Left   (2); np.y := Inv (rp.x);
              when others =>
                null;
            end case;

          when input =>
            case Locate_Cube_Face (p, data) is
              when 6 => From_Top   (2); np.x := rp.x;
              when 5 => From_Right (6); np.y := rp.x;
              when 2 => From_Right (3); np.y := rp.x;
              when others =>
                null;
            end case;
        end case;
        Relative_to_Face (np, new_face, data, -1);
      end Cube_Crawl_through_Bottom_Side;

    begin
      if walking_steps = 0 then
        return;
      end if;
      if verbosity_level > 0 then
        Put (p.x, 3); Put (","); Put (p.y, 3); Put ("  -> (");
        Put (walking_steps, 0); Put (") -> ");
      end if;
      d.x := +Integer (dxr);
      d.y := -Integer (dyr);  --  top down geometry...
      loop
        if verbosity_level > 0 then
          Put ('.');
        end if;
        nd := d;
        np.x := p.x + d.x;
        np.y := p.y + d.y;

        if d.x < 0 then
          if np.x < 1 or else map (np.x, np.y) = ' ' then
            case part is
              when part_1 => Wrap_around_through_Left_Side;
              when part_2 => Cube_Crawl_through_Left_Side;
            end case;
          end if;
        end if;
        if d.x > 0 then
          if np.x > highest.x or else map (np.x, np.y) = ' ' then
            case part is
              when part_1 => Wrap_around_through_Right_Side;
              when part_2 => Cube_Crawl_through_Right_Side;
            end case;
          end if;
        end if;
        if d.y < 0 then
          if np.y < 1 or else map (np.x, np.y) = ' ' then
            case part is
              when part_1 => Wrap_around_through_Top_Side;
              when part_2 => Cube_Crawl_through_Top_Side;
            end case;
          end if;
        end if;
        if d.y > 0 then
          if np.y > highest.y or else map (np.x, np.y) = ' ' then
            case part is
              when part_1 => Wrap_around_through_Bottom_Side;
              when part_2 => Cube_Crawl_through_Bottom_Side;
            end case;
          end if;
        end if;

        --  Check new point
        if map (np.x, np.y) = '.' then
          p := np;
          d := nd;
        else
          exit;
        end if;
        walking_steps := walking_steps - 1;
        exit when walking_steps = 0;
      end loop;
      dxr := Real (+d.x);
      dyr := Real (-d.y);
      if verbosity_level > 0 then
         Put (p.x, 3); Put (","); Put (p.y, 3); New_Line;
      end if;
    end Go;
  begin
    p := start;
    dxr := 1.0;
    dyr := 0.0;
    left.x := -1;
    left.y :=  0;
    right.x := +1;
    right.y :=  0;
    up.x :=  0;
    up.y := -1;
    down.x :=  0;
    down.y := +1;
    if verbosity_level > 1 then
      Put_Line (+"highest.x: " & highest.x'Image);
      Put_Line (+"highest.y: " & highest.y'Image);
      Put_Line (+"side: " & side_length'Image);
    end if;
    walking_steps := 0;
    c := ' ';
    for pos in 1 .. Length (path) loop
      co := c;
      c := Element (path, pos);
      case c is
        when '0' .. '9' =>
          if co in '0' .. '9' then
            walking_steps := walking_steps * 10 + Ord (c) - Ord ('0');
          else
            walking_steps :=                      Ord (c) - Ord ('0');
          end if;
        when 'L' =>
          Go;
          Rotate (dxr, dyr, Pi * 0.5);
        when 'R' =>
          Go;
          Rotate (dxr, dyr, -Pi * 0.5);
        when others =>
          null;
      end case;
    end loop;
    Go;
    final := p;
  end Run;

  data_line : VString;

  procedure Data_Acquisition (data : Data_Type; start : out Point) is
    c : Character;
    f : File_Type;
    p : Point;
  begin
    lowest.x := 1;
    lowest.y := 1;
    highest.x := 1;
    highest.y := 0;
    start.x := 0;
    start.y := 1;
    Clear;
    case data is
      when mini =>
        Open (f, "mini.txt");
      when input =>
        Open (f, "aoc_2022_22.txt");
    end case;
  Read_Data :
    while not End_Of_File (f) loop
      Get_Line (f, data_line);
      exit when data_line = "";
      p.y := highest.y + 1;
      for x in 1 .. Length (data_line) loop
        c :=  Element (data_line, x);
        if start.x = 0 and then c /= ' ' then
          start.x := x;
        end if;
        p.x := x;
        map (p.x, p.y) := c;
      end loop;
      Adapt_Highest_Value_Point (p);
    end loop Read_Data;
    Get_Line (f, data_line);
    if verbosity_level > 0 then
      Show;
    end if;
    Close (f);
  end Data_Acquisition;

  T0 : constant Time := Clock;
  r : array (Part_Type) of Integer;

  data : constant Data_Type := input;
  start, dir, final : Point;
  facing : Natural;

begin
  cube_side (mini)  := 4;
  cube_side (input) := 50;
  Data_Acquisition (data, start);

Parts :
  for part in Part_Type loop
    if verbosity_level > 0 then
      Put_Line (part'Image);
    end if;
    Run (part, data, data_line, start, dir, final);
    if dir.x > 0 then facing := 0; end if;
    if dir.y > 0 then facing := 1; end if;
    if dir.x < 0 then facing := 2; end if;
    if dir.y < 0 then facing := 3; end if;
    r (part) := 1000 * final.y + 4 * final.x + facing;
    if verbosity_level > 0 then
      Put_Line (r (part)'Image);
    end if;
  end loop Parts;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (part_1) /= Integer'Value (To_String (Argument (1))) or
       r (part_2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Final password after a walk; edges are...");
    Put_Line (+"  (part 1) wrapped around . . . . :" & r (part_1)'Image);
    Put_Line (+"  (part 2) on a cube's surface  . :" & r (part_2)'Image);
    --  Part 1: validated by AoC: 26558
    --  Part 2: validated by AoC: 110400
  end if;
end AoC_2022_22;
