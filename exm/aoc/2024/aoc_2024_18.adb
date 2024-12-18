--  Solution to Advent of Code 2024, Day 18
-------------------------------------------
--  RAM Run
--
--  https://adventofcode.com/2024/day/18
--  Copy of questions in: aoc_2024_18_questions.txt
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

procedure AoC_2024_18 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 6; stop : constant := 12;
  input_name : constant VString := +"aoc_2024_18"; n : constant := 70; stop : constant := 1024;

  type Map_Type is array (0 .. n, 0 .. n) of Character;

  map, map_clear : Map_Type;  --  map_clear : emulate Full Ada's `(others => others => '.'))`

  --  Dijkstra shortest path algorithm.
  --  Code adapted from AoC_2024_16.
  --
  --  The following definitions belong to the Dijkstra algorithm, but
  --  we keep them less local because of the path tracking.

  list_length_max : constant := 500_000;
  subtype List_Range is Integer range 1 .. list_length_max;

  type State_Type is record
    pt  : Point;
  end record;

  type Node is record
    len   : Natural;
    state : State_Type;
    pred  : Natural;  --  This is for backtracking the path.
  end record;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  type Score_Type is array (0 .. n, 0 .. n) of Natural;
  best, best_clear : Score_Type;
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
      vec.x := 0;
      vec.y := 0;
      case dir is
        when north => vec.y := -1;
        when east  => vec.x := +1;
        when south => vec.y := +1;
        when west  => vec.x := -1;
      end case;
      s.pt.x := cur_s.pt.x + vec.x;
      if s.pt.x in 0 .. n then
        s.pt.y := cur_s.pt.y + vec.y;
        if s.pt.y in 0 .. n then
          if map (s.pt.x, s.pt.y) /= '.' then
            return;
          end if;
          len_to := cur_len + 1;  --  Cost for a move.
          if len_to < best (s.pt.x, s.pt.y) then
            --  Found a better path to target state s.
            best (s.pt.x, s.pt.y) := len_to;
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
    best     := best_clear;

    cur_s.pt := start;
    cur_len  := 0;

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
      cur_len := best (cur_s.pt.x, cur_s.pt.y);
    end loop;
    return best (cur_s.pt.x, cur_s.pt.y);
  end Dijkstra_Algorithm;

  block : array (1 .. 5000) of Point;
  last_block : Natural := 0;

  r : array (Part_Type) of VString;

  procedure Read_Data is
    sep : Character;
    f : File_Type;
  begin

    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      last_block := last_block + 1;
      Get (f, block (last_block).x);
      Get (f, sep);
      Get (f, block (last_block).y);
    end loop;
    Close (f);

    for y in 0 .. n loop
      for x in 0 .. n loop
        map_clear (x, y) := '.';
        best_clear (x, y) := inf;
      end loop;
    end loop;

  end Read_Data;

  procedure Do_Part_1 is
    s, e : Point;
  begin
    map := map_clear;
    for step in 1 .. stop loop
      map (block (step).x, block (step).y) := '#';
    end loop;
    s.x := 0;
    s.y := 0;
    e.x := n;
    e.y := n;
    r (part_1) := +"" & Dijkstra_Algorithm (s, e);
  end Do_Part_1;

  procedure Do_Part_2 is
    s, e : Point;
    a, b, mid  : Integer;
  begin
    a := stop;
    b := last_block;
    loop
      mid := (a + b) / 2;
      map := map_clear;
      for i in 1 .. mid loop
        map (block (i).x, block (i).y) := '#';
      end loop;
      s.x := 0;
      s.y := 0;
      e.x := n;
      e.y := n;
      if Dijkstra_Algorithm (s, e) = inf then
        b := mid;
      else
        a := mid;
      end if;
      exit when abs (a - b) <= 1;
    end loop;
    r (part_2) := +"" & block (b).x & ',' & block (b).y;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;

  Do_Part_1;
  Do_Part_2;

  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 298 (mini: 22)
    --  Part 2: validated by AoC: 52,32 (mini: 6,1)
  end if;
end AoC_2024_18;
