--  Solution to Advent of Code 2023, Day 17
-------------------------------------------
--  Clumsy Crucible
--
--  https://adventofcode.com/2023/day/17
--  Copy of questions in: aoc_2023_17_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_17 is

  use AoC_Toolbox, HAT;

  input_name : constant VString := +"aoc_2023_17_questions";
  n : constant := 13; skip_header : constant := 20;

  --  GNAT runs the actual problem in 1.55 seconds, while HAC takes forever.
  --
  --  input_name : constant VString := +"aoc_2023_17";
  --  n : constant := 141; skip_header : constant := 0;

  inf : constant Natural := Integer'Last / 4;

  map : array (1 .. n, 1 .. n) of Natural;

  procedure Read_Data is
    c : Character;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    if skip_header > 0 then
      Skip_Line (f, skip_header);
    end if;
    for y in 1 .. n loop
      for x in 1 .. n loop
        Get (f, c);
        map (x, y) := Ord (c) - Ord ('0');
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  --  Dijkstra shortest path algorithm.
  --  Code adapted from AoC_2022_12.
  --
  --  The following definitions belong to the Dijkstra algorithm, but
  --  we keep them less local because of the visualization.

  list_length_max : constant := 500_000;
  subtype List_Range is Integer range 1 .. list_length_max;

  steps_max : constant := 10;

  type State_Type is record
    pt    : Point;
    dir   : Direction_or_Nil;
    steps : Natural;
  end record;

  type Node is record
    len   : Natural;
    state : State_Type;
    pred  : Natural;  --  This is just for displaying the path.
  end record;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  best : array (1 .. n, 1 .. n, Direction, 1 .. steps_max) of Natural;

  verbose : constant Boolean := False;

  function Dijkstra_Algorithm
    (start, finish : Point;
     part          : Part_Type)
  return Natural
  is

    cur_len : Natural;
    cur_s   : State_Type;
    s       : State_Type;  --  Test state dervied from current state.

    procedure Visit (dir : Direction) is
      len_to, ins : Integer;
      ok_step : Boolean;
      new_node : Node;
      vec : Point;
      --  Jump over x, y positions that represent invalid states:
      jump : Positive;
    begin
      case dir is
        when north => vec.x :=  0; vec.y := -1;
        when east  => vec.x := +1; vec.y :=  0;
        when south => vec.x :=  0; vec.y := +1;
        when west  => vec.x := -1; vec.y :=  0;
      end case;
      s := cur_s;
      s.dir := dir;
      if dir = cur_s.dir then
        s.steps := cur_s.steps + 1;
        jump := 1;
      else
        --  Turn.
        case part is
          when part_1 =>
            s.steps := 1;
            jump := 1;
          when part_2 =>
            s.steps := 4;
            jump := 4;
        end case;
      end if;
      s.pt.x := s.pt.x + jump * vec.x;
      s.pt.y := s.pt.y + jump * vec.y;
      if s.pt.x in 1 .. n and then s.pt.y in 1 .. n then
        --  The number of steps in a single direction is limited:
        case part is
          when part_1 =>
            ok_step := s.steps <= 3;
          when part_2 =>
            ok_step := s.steps <= 10;
        end case;
        ok_step :=
          ok_step and then
            --  Avoid gaming the rules by going back and forth (U-turns).
            --  Turns must be 90 degrees.
            (cur_s.dir = nil or else s.dir /= Opposite (cur_s.dir));
        if ok_step then
          len_to := cur_len;
          for count in 1 .. jump loop
            len_to := len_to + map (cur_s.pt.x + count * vec.x, cur_s.pt.y + count * vec.y);
          end loop;
          if len_to < best (s.pt.x, s.pt.y, s.dir, s.steps) then
            --  Found a bette path to target state s.
            best (s.pt.x, s.pt.y, s.dir, s.steps) := len_to;
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
          for s in 1 .. steps_max loop
            best (x, y, d, s) := inf;
          end loop;
        end loop;
      end loop;
    end loop;

    --  Startup state (without a direction) is not visited again.
    --  best (start.x, start.y, nil, 1) := 0;

    cur_s.pt    := start;
    cur_s.dir   := nil;
    cur_s.steps := 1;
    cur_len     := 0;

    loop
      Visit (west);
      Visit (east);
      Visit (north);
      Visit (south);
      --
      --  Switch to the next best explored point.
      --
      current := current + 1;
      if current > explored then
        Put_Line ("No way found.");
        return inf;
      end if;
      cur_s := list (current).state;
      exit when cur_s.pt.x = finish.x and then cur_s.pt.y = finish.y;
      cur_len := best (cur_s.pt.x, cur_s.pt.y, cur_s.dir, cur_s.steps);
    end loop;
    if verbose then
      Put_Line
        ("Final. Dir = " & cur_s.dir'Image & "; steps =" & cur_s.steps'Image);
    end if;
    return best (cur_s.pt.x, cur_s.pt.y, cur_s.dir, cur_s.steps);
  end Dijkstra_Algorithm;

  procedure Show_Path is
    char_map : array (1 .. n, 1 .. n) of Character;
    i, j, len : Natural;
    pt : Point;
  begin
    for y in 1 .. n loop
      for x in 1 .. n loop
        char_map (x, y) := Chr (map (x, y) + Ord ('0'));
      end loop;
    end loop;
    --  Display optimal path, back from the end (E).
    i := current;
    len := list (i).len;
    while i /= 0 loop
      j := list (i).pred;
      pt := list (i).state.pt;
      Put_Line
        (+"Node " & i &
          ": " & pt.x & ", " & pt.y &
          ";  steps: " & list (i).state.steps &
          ", towards " & list (i).state.dir'Image &
          ", cumulated heat loss: " & len);
      len := len - map (pt.x, pt.y);
      if i = current then
        char_map (pt.x, pt.y) := 'E';
      else
        for k in 0 .. list (i).state.steps - 1 loop  --  Backtrack the steps.
          case list (i).state.dir is
            when nil   => char_map (pt.x, pt.y) := '*';
            when north => char_map (pt.x, pt.y + k) := '^';
            when south => char_map (pt.x, pt.y - k) := 'v';
            when east  => char_map (pt.x - k, pt.y) := '>';
            when west  => char_map (pt.x + k, pt.y) := '<';
          end case;
        end loop;
      end if;
      i := j;
    end loop;
    for y in 1 .. n loop
      for x in 1 .. n loop
        Put (char_map (x, y));
      end loop;
      New_Line;
    end loop;
  end Show_Path;

  r : array (Part_Type) of Integer;

  procedure Do_Part (part : Part_Type) is
    start, finish : Point;
  begin
    start.x := 1;
    start.y := 1;
    finish.x := n;
    finish.y := n;
    r (part) := Dijkstra_Algorithm (start, finish, part);
    if verbose then
      Show_Path;
    end if;
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  for p in Part_Type loop
    Do_Part (p);
  end loop;
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
    --  Part 1: validated by AoC: 638  (example: 102)
    --  Part 2: validated by AoC: 748  (example: 94)
  end if;
end AoC_2023_17;
