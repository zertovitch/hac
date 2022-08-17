--  Solution to Advent of Code 2020, Day 24
-------------------------------------------
--  Lobby Layout
--
--  https://adventofcode.com/2020/day/24
--
--  NB: Was able to recycle parts from another "Game of Life"
--      puzzle: Seating System (Day 11).
--      As an hommage to John Conway, there are three
--      "Game of Life" puzzles: Day 11, 17, 24.
--
with HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

procedure AoC_2020_24 is

  type Colour is (black, white);

  function Flip (c : Colour) return Colour is
  begin
    if c = white then
      return black;
    else
      return white;
    end if;
  end Flip;

  type Direction is (e, se, sw, w, nw, ne);

  type Position is record x, y : Integer; end record;

  move : array (Direction) of Position;

  --        (0, 1)    (1, 1)
  --           nw      ne
  --             \    /
  --  (-1, 0) w--(0, 0)--e (1, 0)
  --             /    \
  --           sw      se
  --       (-1,-1)    (0,-1)

  --  Full Ada: we define `move` in a single expression:

  --  move : constant array (Direction) of Position :=
  --          (
  --                nw =>  (0,  1),    ne =>  (1,  1),
  --
  --              w => (-1,  0),             e => (1,  0),
  --
  --                sw => (-1, -1),    se =>  (0, -1)
  --          );

  max : constant := 70;
  subtype Tile_Range is Integer range -max .. max;
  type Map_Type is array (Tile_Range, Tile_Range) of Colour;

  procedure Change (current_map : in Map_Type; new_map : out Map_Type) is
    function Count_Black (x, y : Tile_Range) return Natural is
      occ : Natural := 0;
      --
      procedure Scan_Direction (dx, dy : Integer) is
        xx : constant Integer := x + dx;
        yy : constant Integer := y + dy;
      begin
        loop
          exit when xx not in Tile_Range;
          exit when yy not in Tile_Range;
          case current_map (xx, yy) is
            when black  => occ := occ + 1; exit;
            when white  => exit;
          end case;
          --  Adjacent only in this problem.
          --  xx := xx + dx;
          --  yy := yy + dx;
        end loop;
      end Scan_Direction;
    begin
      for d in Direction loop
        Scan_Direction (move (d).x, move (d).y);
      end loop;
      return occ;
    end Count_Black;
    --
    count : Natural;
  begin
    for x in Tile_Range loop
      for y in Tile_Range loop
        new_map (x, y) := current_map (x, y);
        count := Count_Black (x, y);
        case current_map (x, y) is
          when black =>
            if count = 0 or else count > 2 then
              new_map (x, y) := white;
            end if;
          when white =>
            if count = 2 then
              new_map (x, y) := black;
            end if;
        end case;
      end loop;
    end loop;
  end Change;

  function Is_large_enough (m : Map_Type) return Boolean is
  begin
    for y in Tile_Range loop
      if m (-max, y) = black or else m (max, y) = black then
        return False;
      end if;
    end loop;
    for x in Tile_Range loop
      if m (x, -max) = black or else m (x, max) = black then
        return False;
      end if;
    end loop;
    return True;
  end Is_large_enough;

  function Count (m : Map_Type) return Natural is
    c : Natural := 0;
  begin
    for x in Tile_Range loop
      for y in Tile_Range loop
        if m (x, y) = black then
          c := c + 1;
        end if;
      end loop;
    end loop;
    return c;
  end Count;

  procedure Init_Move is
  --  This way of doing initialization is needed for HAC.
  begin
    move (e).x :=  1;
    move (e).y :=  0;
    move (se).x :=  0;
    move (se).y := -1;
    move (sw).x := -1;
    move (sw).y := -1;
    move (w).x := -1;
    move (w).y :=  0;
    move (nw).x :=  0;
    move (nw).y :=  1;
    move (ne).x :=  1;
    move (ne).y :=  1;
  end Init_Move;

  use HAT;

  procedure Read_Data (m : out Map_Type) is
    --  Initializations are just for removing warnings
    --  issued by the ObjectAda compiler.
    c, c2 : Character := ' ';
    i : Integer;
    f : File_Type;
    s : VString;
    d : Direction := e;
    p : Position;
  begin
    for x in Tile_Range loop
      for y in Tile_Range loop
        m (x, y) := white;
      end loop;
    end loop;
    --
    Open (f, "aoc_2020_24.txt");  --  "input.txt"
    while not End_Of_File (f) loop
      Get_Line (f, s);
      p.x := 0;
      p.y := 0;
      i := 1;
      while i <= Length (s) loop
        c := Element (s, i);
        if c = 'n' or c = 's' then
          i := i + 1;
          c2 := Element (s, i);
        end if;
        i := i + 1;
        case c is
          when 'e' => d := e;
          when 'w' => d := w;
          when 'n' => if c2 = 'e' then d := ne; else d := nw; end if;
          when 's' => if c2 = 'e' then d := se; else d := sw; end if;
          when others => null;
        end case;
        p.x := p.x + move (d).x;
        p.y := p.y + move (d).y;
      end loop;
      m (p.x, p.y) := Flip (m (p.x, p.y));
    end loop;
    Close (f);
  end Read_Data;

  rounds, count_1, count_2 : Natural;
  paving : array (Boolean) of Map_Type;
  state : Boolean := False;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;

begin
  Init_Move;
  Read_Data (paving (state));
  --
  --  Part 1, count black tiles
  --
  count_1 := Count (paving (state));
  --
  --  Part 2, hexagonal game of life
  --
  if compiler_test_mode then
    rounds := 2;
  else
    rounds := 100;
  end if;
  --
  for round in 1 .. rounds loop
    if Is_large_enough (paving (state)) then
      Change (paving (state), paving (not state));
    else
      Put (+"  Too small! max=" & max);
      return;
    end if;
    state := not state;
    if verbose then
      Put_Line (+"Round " & round & ": " & Count (paving (state)));
    end if;
  end loop;
  --
  count_2 := Count (paving (state));
  --
  if compiler_test_mode then
    if count_1 /= Integer_Value (Argument (1)) or
       count_2 /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1 : number of black tiles at startup: " & count_1);
    Put_Line (+"Part 2 : number of black tiles after " & rounds & " rounds: " & count_2);
    --  Part 1: validated by AoC: 341
    --  Part 2: validated by AoC: 3700
  end if;
end AoC_2020_24;
