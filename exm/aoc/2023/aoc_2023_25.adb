--  Solution to Advent of Code 2023, Day 25
-------------------------------------------
--  Snowverload
--
--  https://adventofcode.com/2023/day/25
--  Copy of questions in: aoc_2023_25_questions.txt
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

procedure AoC_2023_25 is

  use AoC_Toolbox, HAT;

  capacity : constant := 1570;

  link : array (1 .. capacity, 1 .. capacity) of Natural;
  --  ^ To save memory, we use the same array for storing
  --    the data's information (0 = no link between i and j)
  --    and for counting visits through edges.

  last : Natural := 0;

  name : array (1 .. capacity) of VString;  --  For display purposes.

  verbosity : constant := 0;

  function Label (i : Positive) return VString is
  begin
    return name (i) & '-' & Image (i);
  end Label;

  input_name   : VString;
  skip_header  : Natural;
  mc_iter      : Positive;
  vertex_limit : Positive;

  procedure Read_Data is
    use Hash_Maps;
    hm : Hash_Map_Type;
    c : Character;
    key : VString;
    src, dst : Integer;

    f : File_Type;

    procedure Get_Key is
    begin
      key := Null_VString;
      loop
        Get (f, c);
        exit when c not in Alpha;
        key := key & c;
        exit when End_Of_Line (f);
      end loop;
    end Get_Key;

  begin
    for i in 1 .. vertex_limit loop
      for j in 1 .. vertex_limit loop
        link (i, j) := 0;
      end loop;
    end loop;
    Clear (hm);
    Open (f, input_name & ".txt");
    for skip_it in 1 .. skip_header loop
      Skip_Line (f);
    end loop;
    while not End_Of_File (f) loop
      Get_Key;
      Insert (hm, key, last + 1, src);
      if src > last then  --  We found a new name
        last := src;
        name (last) := key;
      end if;
      if verbosity >= 2 then
        Put (Label (src) & ": ");
      end if;
      Get (f, c);
      loop
        Get_Key;
        Insert (hm, key, last + 1, dst);
        if dst > last then  --  We found a new name
          last := dst;
          name (last) := key;
        end if;
        if verbosity >= 2 then
          Put (Label (dst) & ' ');
        end if;
        link (src, dst) := 1;
        link (dst, src) := 1;
        exit when End_Of_Line (f);
      end loop;
      if verbosity >= 2 then
        New_Line;
      end if;
      exit when End_Of_File (f);
      Skip_Line (f);
      exit when End_Of_Line (f);  --  Blank line after the data.
    end loop;
    Close (f);
  end Read_Data;

  --  Dijkstra stuff.

  list_length_max : constant := 500_000;
  subtype List_Range is Integer range 1 .. list_length_max;

  subtype State_Type is Positive;

  type Node is record
    len   : Natural;
    state : State_Type;
    pred  : Natural;
    --  `pred` is for backtracking the shortest path, to
    --  increment the edges counters.
  end record;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  inf : constant Natural := Integer'Last / 4;

  function Dijkstra_Algorithm (start, finish : Positive) return Natural
  is
    best : array (1 .. capacity) of Natural;

    cur_len : Natural;
    cur_s   : State_Type;

    procedure Visit (new_vertex : Positive) is
      len_to, ins : Integer;
      new_node : Node;
    begin
      len_to := cur_len + 1;  --  All edge have a length 1.
      if len_to < best (new_vertex) then
        --  Found a better path to target state.
        best (new_vertex) := len_to;
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
        new_node.state := new_vertex;
        new_node.pred  := current;
        list (ins) := new_node;
        explored := explored + 1;
      end if;
    end Visit;

  begin
    current  := 0;
    explored := 0;

    for x in 1 .. last loop
      best (x) := inf;
    end loop;

    cur_s   := start;
    cur_len := 0;

    loop
      for j in 1 .. last loop
        if link (cur_s, j) > 0 then
          Visit (j);
        end if;
      end loop;
      --
      --  Switch to the next best explored point.
      --
      current := current + 1;
      if current > explored then
        Put_Line ("No way found.");
        return inf;
      end if;
      cur_s := list (current).state;
      exit when cur_s = finish;
      cur_len := best (cur_s);
    end loop;
    return best (cur_s);
  end Dijkstra_Algorithm;

  busiest : Positive := 1;

  --  We walk from each vertex to another, randomly chosen, vertex.
  --  On the way, we increment the visited vertices' counts by 1.
  --
  procedure Walk_Through_Randomly is

    procedure Mark_Edge (vertex_1, vertex_2 : Positive) is
      busy : Natural;
    begin
      if verbosity >= 3 then
        Put_Line
          (+"  edge " & Label (vertex_1) & " to " & Label (vertex_2));
      end if;
      busy := link (vertex_1, vertex_2) + 1;
      link (vertex_1, vertex_2) := busy;
      link (vertex_2, vertex_1) := busy;
      busiest := Max (busiest, busy);
    end Mark_Edge;

    len : Natural;
    j, k : Positive;
  begin
    for i in 1 .. last loop
      loop
        j := 1 + Rand (last - 1);
        exit when j /= i;
      end loop;
      len := Dijkstra_Algorithm (i, j);
      if verbosity >= 2 then
        Put_Line (Label (i) & " -> " & Label (j) & ": " & len);
        if verbosity >= 3 then
          for lp in 1 .. current loop
            Put_Line
              (+"  list item " & lp & ": vertex: " &
               Label (list (lp).state) &
               ", predecessor item " & list (lp).pred);
          end loop;
        end if;
      end if;
      --  Back-track from j to i
      loop
        current := list (current).pred;
        if current = 0 then
          Mark_Edge (i, j);
          exit;
        end if;
        k := list (current).state;
        Mark_Edge (j, k);
        j := k;
      end loop;
    end loop;
  end Walk_Through_Randomly;

  procedure Show_Busiest is
    count : Natural := 0;
    top_x : constant := 5;
  begin
  Outer :
    for busy in reverse 1 .. busiest loop
      for i in 1 .. last loop
        for j in 1 .. i - 1 loop
          if link (i, j) = busy then
            Put_Line (+"   " & Label (i) & " -> " & Label (j) & ": " & busy);
            count := count + 1;
            exit Outer when count = top_x;
          end if;
        end loop;
      end loop;
    end loop Outer;
  end Show_Busiest;

  procedure Show_Graph_Stats is
    count : Natural := 0;
  begin
    Put_Line (+"Vertices : " & last);
    for busy in reverse 1 .. busiest loop
      for i in 1 .. last loop
        for j in 1 .. i - 1 loop
          if link (i, j) > 0 then
            count := count + 1;
          end if;
        end loop;
      end loop;
    end loop;
    Put_Line (+"Edges    : " & count);
  end Show_Graph_Stats;

  procedure Remove_Busiest_Edge is
    count : Natural := 0;
    busiest_i, busiest_j : Positive;
  begin
  Outer :
    for i in 1 .. last loop
      for j in 1 .. i - 1 loop
        if link (i, j) = busiest then
          busiest_i := i;
          busiest_j := j;
          count := count + 1;
          exit Outer when count = 2;
        end if;
      end loop;
    end loop Outer;
    if count = 2 then
      Put_Line ("!! At least two ex-eaquo busiest");
    end if;
    link (busiest_i, busiest_j) := 0;
    link (busiest_j, busiest_i) := 0;
  end Remove_Busiest_Edge;

  procedure Reset_Counts is
  begin
    for i in 1 .. last loop
      for j in 1 .. last loop
        if link (i, j) > 0 then
          link (i, j) := 1;
        end if;
      end loop;
    end loop;
    busiest := 1;
  end Reset_Counts;

  function Count_any_Group_Size return Integer is
    visited : array (1 .. capacity) of Boolean;
    counter : Natural := 0;
    procedure Visit_from (i : Positive) is
    begin
      if not visited (i) then
        visited (i) := True;
        counter := counter + 1;
        for j in 1 .. last loop
          if link (i, j) > 0 then
            Visit_from (j);
          end if;
        end loop;
      end if;
    end Visit_from;
  begin
    for i in 1 .. last loop
      visited (i) := False;
    end loop;
    Visit_from (1);
    return counter;
  end Count_any_Group_Size;

  answer : Integer;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Random_Seed (1);
  --  ^ We want a reproduceable result, even though
  --    mc_iter is sufficient *most of the time* when
  --    the seed is randomized...

  if compiler_test_mode then
    --  GNAT runs this program on the actual problem
    --  data in 3 seconds, while HAC takes forever.
    input_name   := +"aoc_2023_25_questions";
    skip_header  := 43;
    mc_iter      := 10;
    vertex_limit := 40;
  else
    input_name   := +"aoc_2023_25";
    skip_header  := 0;
    mc_iter      := 1;
    vertex_limit := capacity;
  end if;

  Read_Data;

  if verbosity >= 1 then
    Show_Graph_Stats;
  end if;

  for edge_removal_count in 1 .. 3 loop
    --  Empirical observation: apart from the busiest edge,
    --  the next top edges in the list of busiest edges are
    --  not reliable candidates for cutting all three wires
    --  at the same time.
    --  Check with `verbosity` >= 1 and by disabling the
    --  fixed random seed (comment out `Random_Seed (1)`).

    for iter in 1 .. mc_iter loop
      if verbosity >= 2 then
        Put_Line (+"Macro iteration " & iter & ", shortest paths");
      end if;
      Walk_Through_Randomly;
    end loop;

    if verbosity >= 1 then
      Put_Line (+"Round " & edge_removal_count & "; busiest edges:");
      Show_Busiest;
    end if;

    Remove_Busiest_Edge;  --  Snip!
    Reset_Counts;

  end loop;

  --  Now, we have done three times this : remove the busiest edge.
  --  AoC promises us that the heap of components can be
  --  split into two separate, disconnected groups by cutting
  --  only three wires.

  answer := Count_any_Group_Size;
  answer := answer * (last - answer);

  if compiler_test_mode then
    if answer /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & answer'Image);
    --  Part 1: validated by AoC: 614655
  end if;
end AoC_2023_25;
