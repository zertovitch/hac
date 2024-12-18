--  Solution to Advent of Code 2024, Day 16
-------------------------------------------
--  Reindeer Maze
--
--  https://adventofcode.com/2024/day/16
--  Copy of questions in: aoc_2024_16_questions.txt
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

procedure AoC_2024_16 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini1"; n : constant := 15;
  --  input_name : constant VString := +"mini2"; n : constant := 17;
  input_name : constant VString := +"aoc_2024_16"; n : constant := 141;

  r : array (Part_Type) of Integer;

  s, e : Point;
  map : array (1 .. n, 1 .. n) of Character;

  procedure Read_Data is
    c : Character;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        Get (f, c);
        case c is
          when 'S' =>
            s.x := x;
            s.y := y;
            c := '.';
          when 'E' =>
            e.x := x;
            e.y := y;
            c := '.';
          when others =>
            null;
        end case;
        map (x, y) := c;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  --  Dijkstra shortest path algorithm.
  --  Code adapted from AoC_2023_17.
  --
  --  The following definitions belong to the Dijkstra algorithm, but
  --  we keep them less local because of the path tracking.

  list_length_max : constant := 500_000;
  subtype List_Range is Integer range 1 .. list_length_max;

  type State_Type is record
    pt  : Point;
    dir : Direction;
  end record;

  type Node is record
    len   : Natural;
    state : State_Type;
    pred  : Natural;  --  This is for backtracking the path.
  end record;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  best : array (1 .. n, 1 .. n, Direction) of Natural;
  inf : constant Natural := Integer'Last / 4;

  function Dijkstra_Algorithm (start, finish : Point) return Natural is
    cur_len : Natural;
    cur_s   : State_Type;
    s       : State_Type;  --  Test state derived from current state.

    procedure Visit (dir : Direction) is
      len_to, ins : Integer;
      vec : Point;
      new_node : Node;
    begin
      if dir = Opposite (cur_s.dir) then
        --  Turns must be 90 degrees.
        return;
      end if;
      vec.x := 0;
      vec.y := 0;
      --  Move (otherwise, turn without moving):
      if cur_s.dir = dir then
        case dir is
          when north => vec.y := +1;
          when east  => vec.x := +1;
          when south => vec.y := -1;
          when west  => vec.x := -1;
        end case;
      end if;
      s.pt.x := cur_s.pt.x + vec.x;
      if s.pt.x in 1 .. n then
        s.pt.y := cur_s.pt.y + vec.y;
        if s.pt.y in 1 .. n then
          if map (s.pt.x, s.pt.y) /= '.' then
            return;
          end if;
          len_to := cur_len;
          if cur_s.dir /= dir then
            len_to := len_to + 1000;  --  Cost for a turn.
          else
            len_to := len_to + 1;     --  Cost for a move.
          end if;
          if len_to < best (s.pt.x, s.pt.y, dir) then
            s.dir := dir;
            --  Found a better path to target state s.
            best (s.pt.x, s.pt.y, dir) := len_to;
            --
            --  Insert in a sorted way.
            --
            ins := explored + 1;
            for i in current + 1 .. explored loop
              if len_to < list (i).len then
                ins := i;  --  Insert here.
                --  Optional: remove another node
                --  with the same state and a larger length.
                exit;
              end if;
            end loop;
            for i in reverse ins .. explored loop
              list (i + 1) := list (i);
            end loop;
            new_node.len   := len_to;
            new_node.state := s;
            new_node.pred  := current;
            list (ins) := new_node;
            explored := explored + 1;
          end if;
        end if;
      end if;
    end Visit;

  begin
    current  := 0;
    explored := 0;

    for x in 1 .. n loop
      for y in 1 .. n loop
        for d in Direction loop
          best (x, y, d) := inf;
        end loop;
      end loop;
    end loop;

    cur_s.pt  := start;
    cur_s.dir := east;
    cur_len   := 0;

    loop
      for d in Direction loop
        Visit (d);
      end loop;
      --
      --  Switch to the next best explored point.
      --
      current := current + 1;
      if current > explored then
        --  Put_Line ("No way found.");
        return inf;
      end if;
      cur_s := list (current).state;
      exit when cur_s.pt.x = finish.x and then cur_s.pt.y = finish.y;
      cur_len := best (cur_s.pt.x, cur_s.pt.y, cur_s.dir);
    end loop;
    return best (cur_s.pt.x, cur_s.pt.y, cur_s.dir);
  end Dijkstra_Algorithm;

  procedure Do_Part_1 is
  begin
    r (part_1) := Dijkstra_Algorithm (s, e);
  end Do_Part_1;

  procedure Show_Map is
  begin
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        if x = s.x and then y = s.y then
          Put ('S');
        elsif x = e.x and then y = e.y then
          Put ('E');
        else
          Put (map (x, y));
        end if;
      end loop;
      New_Line;
    end loop;
    New_Line;
  end Show_Map;

  verbose : constant Boolean := False;

  --  For part 2 you have to find all optimal paths.
  --  The method below is certainly far from being the most efficient
  --  but at least maybe you will understand it ;-) ...
  --
  procedure Do_Part_2 is
    opt : constant Integer := r (part_1);
    seat : array (1 .. n, 1 .. n) of Boolean;

    procedure Search_Alternatives is
      path : array (1 .. 20_000) of Point;
      last : Natural := 0;
      block : Point;

      procedure Record_Path is
        --  We record the optimal path found by latest
        --  run of Dijkstra's algorithm.
        i : Integer := current;
      begin
        last := 0;
        while i /= 0 loop
          if i = current
            or else not
              (list (i).state.pt.x = path (last).x and then
               list (i).state.pt.y = path (last).y)
          then
            last := last + 1;
            path (last) := list (i).state.pt;
            seat (path (last).x, path (last).y) := True;
          end if;
          i := list (i).pred;
        end loop;
      end Record_Path;

    begin
      Record_Path;
      for i in 2 .. last - 1 loop
        block := path (i);
        --  Put a road block on the optimal way, step #i, and see what happens.
        map (block.x, block.y) := 'B';
        if Dijkstra_Algorithm (s, e) = opt then
          --  There is another optimal path despite the road block.
          Search_Alternatives;
        else
          --  The road block worsens the optimal path. Remove it.
          map (block.x, block.y) := '.';
        end if;
      end loop;
    end Search_Alternatives;

  begin

    for x in 1 .. n loop
      for y in 1 .. n loop
        seat (x, y) := False;
      end loop;
    end loop;

    Search_Alternatives;

    r (part_2) := 0;
    for x in 1 .. n loop
      for y in 1 .. n loop
        if seat (x, y) then
          r (part_2) := r (part_2) + 1;
        end if;
      end loop;
    end loop;

    if verbose then
      Show_Map;
    end if;

  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;

  Do_Part_1;
  if not compiler_test_mode then
    Do_Part_2;
  end if;

  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 74392
    --  Part 2: validated by AoC: 426
  end if;
end AoC_2024_16;
