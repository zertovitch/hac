--  Solution to Advent of Code 2024, Day 15
-------------------------------------------
--  Warehouse Woes
--
--  https://adventofcode.com/2024/day/15
--  Copy of questions in: aoc_2024_15_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_15 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"micro"; n : constant := 6; nx : constant := 12; nm : constant := 1;
  --  input_name : constant VString := +"mini0"; n : constant := 7; nx : constant := 14; nm : constant := 11;
  --  input_name : constant VString := +"mini1"; n : constant := 8; nx : constant := 16; nm : constant := 15;
  --  input_name : constant VString := +"mini2"; n : constant := 10; nx : constant := 20; nm : constant := 700;
  input_name : constant VString := +"aoc_2024_15"; n : constant := 50; nx : constant := 100; nm : constant := 20_000;

  map  : array (1 .. n, 1 .. n) of Character;
  mapx : array (1 .. nx, 1 .. n) of Character;
  move : array (1 .. nm) of Direction;
  p0, p : Point;

  r : array (Part_Type) of Integer;

  procedure Read_Data is
    new_tile : String (1 .. 2);
    c : Character;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        Get (f, c);
        if c = '@' then
          p0.x := x;
          p0.y := y;
          c := '.';
        end if;
        map (x, y) := c;
        case c is
          when '#' => new_tile := "##";
          when 'O' => new_tile := "[]";
          when '.' => new_tile := "..";
          when others => null;
        end case;
        mapx ((x - 1) * 2 + 1, y) := new_tile (1);
        mapx ((x - 1) * 2 + 2, y) := new_tile (2);
      end loop;
    end loop;
    Skip_Line (f);
    for i in 1 .. nm loop
      Get (f, c);
      case c is
        when '^' => move (i) := north;
        when 'v' => move (i) := south;
        when '>' => move (i) := east;
        when '<' => move (i) := west;
        when others => null;
      end case;
    end loop;
    Close (f);
  end Read_Data;

  function Count return Natural is
    c : Natural := 0;
  begin
    for y in 1 .. n loop
      for x in 1 .. n loop
        if map (x, y) = 'O' then
          c := c + 1;
        end if;
      end loop;
    end loop;
    return c;
  end Count;

  procedure Show_Map is
  begin
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        if x = p.x and then y = p.y then
          Put ('@');
        else
          Put (map (x, y));
        end if;
      end loop;
      New_Line;
    end loop;
    Put ("Boxes: " & Integer'Image (Count));
    New_Line;
  end Show_Map;

  verbose : constant Boolean := False;

  procedure Do_Part_1 is
  begin
    p := p0;

    for m in 1 .. nm loop
      case move (m) is
        when north =>
          case map (p.x, p.y + 1) is
            when '.' => p.y := p.y + 1;
            when 'O' =>
              for y in p.y + 2 .. n loop
                exit when map (p.x, y) = '#';
                if map (p.x, y) = '.' then
                  for my in reverse p.y + 2 .. y loop
                    map (p.x, my) := map (p.x, my - 1);
                  end loop;
                  map (p.x, p.y + 1) := '.';
                  p.y := p.y + 1;
                  exit;
                end if;
              end loop;
            when others => null;
          end case;

        when south =>
          case map (p.x, p.y - 1) is
            when '.' => p.y := p.y - 1;
            when 'O' =>
              for y in reverse 1 .. p.y - 2 loop
                exit when map (p.x, y) = '#';
                if map (p.x, y) = '.' then
                  for my in y .. p.y - 2 loop
                    map (p.x, my) := map (p.x, my + 1);
                  end loop;
                  map (p.x, p.y - 1) := '.';
                  p.y := p.y - 1;
                  exit;
                end if;
              end loop;
            when others => null;
          end case;

        when east =>
          case map (p.x + 1, p.y) is
            when '.' => p.x := p.x + 1;
            when 'O' =>
              for x in p.x + 2 .. n loop
                exit when map (x, p.y) = '#';
                if map (x, p.y) = '.' then
                  for mx in reverse p.x + 2 .. x loop
                    map (mx, p.y) := map (mx - 1, p.y);
                  end loop;
                  map (p.x + 1, p.y) := '.';
                  p.x := p.x + 1;
                  exit;
                end if;
              end loop;
            when others => null;
          end case;

        when west =>
          case map (p.x - 1, p.y) is
            when '.' => p.x := p.x - 1;
            when 'O' =>
              for x in reverse 1 .. p.x - 2 loop
                exit when map (x, p.y) = '#';
                if map (x, p.y) = '.' then
                  for mx in x .. p.x - 2 loop
                    map (mx, p.y) := map (mx + 1, p.y);
                  end loop;
                  map (p.x - 1, p.y) := '.';
                  p.x := p.x - 1;
                  exit;
                end if;
              end loop;
            when others => null;
          end case;
      end case;
    end loop;

    for y in 1 .. n loop
      for x in 1 .. n loop
        if map (x, y) = 'O' then
          r (part_1) := r (part_1) + 100 * (n - y) + (x - 1);
        end if;
      end loop;
    end loop;

    if verbose then
      Show_Map;
    end if;

  end Do_Part_1;

  procedure Show_Map_X is
  begin
    for y in reverse 1 .. n loop
      for x in 1 .. nx loop
        if x = p.x and then y = p.y then
          Put ('@');
        else
          Put (mapx (x, y));
        end if;
      end loop;
      New_Line;
    end loop;
    --  Put ("Boxes: " & Integer'Image (Count_X));
    New_Line;
  end Show_Map_X;

  procedure Do_Part_2 is

    function Can_Move_Box (bx, by, dy : Integer) return Boolean is
      ok : Boolean := True;
    begin
      for dx in 0 .. 1 loop
        case mapx (bx + dx, by + dy) is
          when '#' => return False;
          when '[' => ok := ok and then Can_Move_Box (bx + dx,     by + dy, dy); exit when dx = 0;
          when ']' => ok := ok and then Can_Move_Box (bx + dx - 1, by + dy, dy);
          when others => null;
        end case;
      end loop;
      return ok;
    end Can_Move_Box;

    procedure Move_Box (bx, by, dy : Integer) is
    begin
      if Can_Move_Box (bx, by, dy) then
        for dx in 0 .. 1 loop
          case mapx (bx + dx, by + dy) is
            when '#' => return;
            when '[' => Move_Box (bx + dx,     by + dy, dy); exit when dx = 0;
            when ']' => Move_Box (bx + dx - 1, by + dy, dy);
            when others => null;
          end case;
        end loop;
        if mapx (bx, by + dy) = '.' and then mapx (bx + 1, by + dy) = '.' then
          mapx (bx,     by + dy) := '[';
          mapx (bx + 1, by + dy) := ']';
          mapx (bx,     by) := '.';
          mapx (bx + 1, by) := '.';
        end if;
      end if;
    end Move_Box;

  begin
    p.x := (p0.x - 1) * 2 + 1;
    p.y := p0.y;

    if verbose then
      Show_Map_X;
    end if;

    for m in 1 .. nm loop
      if verbose then
        Put_Line (Direction'Image (move (m)));
      end if;

      case move (m) is
        when north =>
          case mapx (p.x, p.y + 1) is
            when '['    => Move_Box (p.x,     p.y + 1, 1);
            when ']'    => Move_Box (p.x - 1, p.y + 1, 1);
            when others => null;
          end case;
          if mapx (p.x, p.y + 1) = '.' then
            p.y := p.y + 1;
          end if;

        when south =>
          case mapx (p.x, p.y - 1) is
            when '['    => Move_Box (p.x,     p.y - 1, -1);
            when ']'    => Move_Box (p.x - 1, p.y - 1, -1);
            when others => null;
          end case;
          if mapx (p.x, p.y - 1) = '.' then
            p.y := p.y - 1;
          end if;

        when east =>
          case mapx (p.x + 1, p.y) is
            when '.' => p.x := p.x + 1;
            when '[' =>
              for x in p.x + 2 .. nx loop
                exit when mapx (x, p.y) = '#';
                if mapx (x, p.y) = '.' then
                  for mx in reverse p.x + 2 .. x loop
                    mapx (mx, p.y) := mapx (mx - 1, p.y);
                  end loop;
                  mapx (p.x + 1, p.y) := '.';
                  p.x := p.x + 1;
                  exit;
                end if;
              end loop;
            when others => null;
          end case;

        when west =>
          case mapx (p.x - 1, p.y) is
            when '.' => p.x := p.x - 1;
            when ']' =>
              for x in reverse 1 .. p.x - 2 loop
                exit when mapx (x, p.y) = '#';
                if mapx (x, p.y) = '.' then
                  for mx in x .. p.x - 2 loop
                    mapx (mx, p.y) := mapx (mx + 1, p.y);
                  end loop;
                  mapx (p.x - 1, p.y) := '.';
                  p.x := p.x - 1;
                  exit;
                end if;
              end loop;
            when others => null;
          end case;
      end case;

      if verbose then
        Show_Map_X;
      end if;

    end loop;

    for y in 1 .. n loop
      for x in 1 .. nx loop
        if mapx (x, y) = '[' then
          r (part_2) := r (part_2) + 100 * (n - y) + (x - 1);
        end if;
      end loop;
    end loop;

  end Do_Part_2;

  procedure Do_Part (part : Part_Type) is
  begin
    null;  --  r (part) := r (part) + 1;
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  Do_Part (part_1);
  Do_Part (part_2);
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
    --  Part 1: validated by AoC: 1515788
    --  Part 2: validated by AoC: 1516544
  end if;
end AoC_2024_15;
