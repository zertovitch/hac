--  Solution to Advent of Code 2024, Day 20
-------------------------------------------
--  Race Condition
--
--  https://adventofcode.com/2024/day/20
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

with Interfaces;

procedure AoC_2024_20 is

  use AoC_Toolbox, HAT;

  input_name : constant VString := +"aoc_2024_20_mini"; n : constant := 15;
  --  input_name : constant VString := +"aoc_2024_20"; n : constant := 141;

  verbose : constant Boolean := False;

  map : array (1 .. n, 1 .. n) of Character;

  --  Dijkstra shortest path algorithm.
  --  Code adapted from AoC_2024_18.
  --
  --  The following definitions belong to the Dijkstra algorithm, but
  --  we keep them less local because of the path tracking.

  list_length_max : constant := 500_000;
  subtype List_Range is Integer range 1 .. list_length_max;

  type State_Type is record
    pt : Point;
  end record;

  type Node is record
    len   : Natural;
    state : State_Type;
    pred  : Natural;  --  This is for backtracking the path.
  end record;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  type Score_Type is array (1 .. n, 1 .. n) of Natural;  --  , 0 .. path_max, Direction
  best, best_clear : Score_Type;
  inf : constant Natural := Integer'Last / 4;

  start, finish, cheat_start, cheat_end : Point;

  function Dijkstra_Algorithm (cheat_start_step : Natural; cheat_dir : Direction_or_Nil) return Natural is
    cur_len : Natural;
    cur_s   : State_Type;
    s       : State_Type;  --  Test state derived from current state.

    procedure Visit (dir : Direction) is
      len_to, ins : Integer;
      new_node : Node;
    begin
      s := cur_s;
      case dir is
        when north => s.pt.y := cur_s.pt.y - 1;
        when east  => s.pt.x := cur_s.pt.x + 1;
        when south => s.pt.y := cur_s.pt.y + 1;
        when west  => s.pt.x := cur_s.pt.x - 1;
      end case;
      len_to := cur_len + 1;
      if s.pt.x in 2 .. n - 1
        and then s.pt.y in 2 .. n - 1
        and then len_to < best (s.pt.x, s.pt.y)
      then
        if map (s.pt.x, s.pt.y) = '.'
          or else (len_to = cheat_start_step and then dir = cheat_dir)
        then
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
    end Visit;

  begin
    current  := 0;
    explored := 0;
    best     := best_clear;
    cheat_end.x := -1;
    cheat_end.y := -1;

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
        Put_Line ("No way found.");
        return inf;
      end if;
      cur_s := list (current).state;
      exit when cur_s.pt.x = finish.x and then cur_s.pt.y = finish.y;
      cur_len := best (cur_s.pt.x, cur_s.pt.y);  --  , cur_s.cheat, cur_s.cheat_dir
    end loop;
    return best (cur_s.pt.x, cur_s.pt.y);
  end Dijkstra_Algorithm;

  r : array (Part_Type) of VString;

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
            start.x := x;
            start.y := y;
            c := '.';
          when 'E' =>
            finish.x := x;
            finish.y := y;
            c := '.';
          when others =>
            null;
        end case;
        map (x, y) := c;
        best_clear (x, y) := inf;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Show_Map is
  begin
    for y in reverse 1 .. n loop
      for x in 1 .. n loop
        if x = start.x and then y = start.y then
          Put ('S');
        elsif x = finish.x and then y = finish.y then
          Put ('E');
        elsif x = cheat_start.x and then y = cheat_start.y then
          Put ('C');
        elsif x = cheat_end.x and then y = cheat_end.y then
          Put ('c');
        else
          Put (map (x, y));
        end if;
      end loop;
      New_Line;
    end loop;
    New_Line;
  end Show_Map;

  stat : array (1 .. 10_000) of Natural;
  compute_stats : constant Boolean := verbose;

  function Do_Part_1_Explicit_Cheat_Point return VString is
    len, count, save : Integer;

    procedure Find_Cheat_Points (cheat_step : Integer) is
      --  We record the optimal path found by latest
      --  run of Dijkstra's algorithm.
      i : Integer := current;
      step : Integer := len;
    begin
      while i /= 0 loop
        i := list (i).pred;
        step := step - 1;
        if step = cheat_step + 1 then
          cheat_end := list (i).state.pt;
        elsif step = cheat_step then
          cheat_start := list (i).state.pt;
          exit;
        end if;
      end loop;
    end Find_Cheat_Points;

    use Interfaces;

    cheats : Hash_Maps.Hash_Map_Type;
    value : Integer_64;
    key : VString;
    unknown : constant := -1;
    path_max : constant Natural := Dijkstra_Algorithm (0, nil);

  begin
    count := 0;

    if compute_stats then
      for i in 1 .. path_max loop
        stat (i) := 0;
      end loop;
    end if;

    Hash_Maps.Clear (cheats);

    for cheat_step in 1 .. path_max loop
      for cheat_dir in Direction loop
        len := Dijkstra_Algorithm (cheat_step, cheat_dir);
        save := path_max - len;
        if save > 0 then
          Find_Cheat_Points (cheat_step);
          key :=
            Image (cheat_start.x) & "," & Image (cheat_start.y) & "x" &
            Image (cheat_end.x) & "," & Image (cheat_end.y);

          Hash_Maps.Find (cheats, key, unknown, value);
          if value = unknown then
            Hash_Maps.Insert (cheats, key, 1, True, value);
            if verbose then
              Put_Line (+"Saved: " & save);
              Show_Map;
            end if;
            if compute_stats then
              stat (save) := stat (save) + 1;
            end if;
            if save >= 100 then
              count := count + 1;
            end if;
          else
            if verbose then
              Put_Line (+"Duplicate for saved: " & save);
              Show_Map;
            end if;
          end if;
        end if;
      end loop;
    end loop;

    --  Finds 1361 cheats with save > 0 including 2 start/end point duplicates -> 1359 unique.
    --  1360 is the correct value...
    if compute_stats and verbose then
      for i in 1 .. path_max loop
        if stat (i) > 0 then
          Put_Line (+"There are " & stat (i) & " cheats that save " & i & " picoseconds.");
        end if;
      end loop;
    end if;

    return +"" & count;

  end Do_Part_1_Explicit_Cheat_Point;

  function Do_as_JC_Moyer (max_cheats : Positive; threshold : Natural) return VString is
    distance : Score_Type;

    --  No need for a shortest path algorithm:
    --  we know there is only one path.
    --
    procedure Build_Distance_Map is
      c    : Node;
      n    : Point;
      last : Natural;
    begin
      distance := best_clear;

      list (1).len := 0;
      list (1).state.pt := start;
      last := 1;

      while last > 0 loop
        c := list (last);
        last := last - 1;

        distance (c.state.pt.x, c.state.pt.y) := c.len;

        for d in Direction loop
          n := c.state.pt;
          case d is
            when north => n.y := c.state.pt.y - 1;
            when east  => n.x := c.state.pt.x + 1;
            when south => n.y := c.state.pt.y + 1;
            when west  => n.x := c.state.pt.x - 1;
          end case;

          if distance (n.x, n.y) = inf and then map (n.x, n.y) = '.' then
            last := last + 1;
            list (last).len := c.len + 1;
            list (last).state.pt := n;
          end if;
        end loop;
      end loop;
    end Build_Distance_Map;

    count : Integer;
    c, n : Point;

    use Interfaces;

    cheats : Hash_Maps.Hash_Map_Type;
    value : Integer_64;
    key : VString;
    save : Integer;

  begin
    Build_Distance_Map;
    Hash_Maps.Clear (cheats);

    for x in map'Range (1) loop
      for y in map'Range (2) loop
        for dx in -max_cheats .. +max_cheats loop
          for dy in -max_cheats .. +max_cheats loop
            if distance (x, y) < inf then
              c.x := x;
              c.y := y;
              n.x := c.x + dx;
              n.y := c.y + dy;
              if n.x in map'Range (1) and then n.y in map'Range (2)
                and then Dist_L1 (n, c) <= max_cheats
                and then distance (c.x, c.y) > distance (n.x, n.y)
                --  Implied pathable; walls are = inf
              then
                key :=
                  Image (c.x) & "," & Image (c.y) & "x" &
                  Image (n.x) & "," & Image (n.y);

                --  Simulate all walls being removed horizontally from (say) c to (n.x, c.y),
                --  then vertically from (n.x, c.y) to n, or even on the whole rectangle with
                --  points c and n as opposite corners. The exact path doesn't matter: all
                --  lengths are the same (L1 / Manhattan distance) since there is no obstacle!

                --  Compute the gain obtained by having the "cheat" path:

                value :=
                  Integer_64 ((distance (c.x, c.y) - distance (n.x, n.y)) - Dist_L1 (n, c));

                Hash_Maps.Insert (cheats, key, value, True, value);
              end if;
            end if;
          end loop;
        end loop;
      end loop;
    end loop;

    count := 0;

    if compute_stats then
      for i in stat'Range loop
        stat (i) := 0;
      end loop;
    end if;

    --  Traverse the hash table for counting the stones:
    for i in cheats'Range loop
      for j in 1 .. cheats (i).slots loop
        save := Integer (cheats (i).slot (j).value);
        if save > 0 then
          if compute_stats then
            stat (save) := stat (save) + 1;
          end if;
          if save >= threshold then
            count := count + 1;
          end if;
        end if;
      end loop;
    end loop;

    if compute_stats and verbose then
      Put_Line (+"Max cheat length: " & max_cheats);
      for i in stat'Range loop
        if stat (i) > 0 then
          Put_Line (+"There are " & stat (i) & " cheats that save " & i & " picoseconds.");
        end if;
      end loop;
    end if;

    return +"" & count;

  end Do_as_JC_Moyer;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

  threshold_part_1, threshold_part_2 : Natural;

  type Method is (m1, m2);
  choice : constant Method := m2;

begin
  Read_Data;

  if Index (input_name, "mini") > 0 then
    threshold_part_1 := 1;
    threshold_part_2 := 50;
  else
    threshold_part_1 := 100;
    threshold_part_2 := 100;
  end if;

  case choice is
    when m1 =>
      r (part_1) := Do_Part_1_Explicit_Cheat_Point;
      r (part_2) := +"";
    when m2 =>
      --  Adapted from the very smart solution @
      --  https://github.com/jcmoyer/puzzles/blob/master/AdventOfCode2024/src/day20.adb
      r (part_1) := Do_as_JC_Moyer (2,  threshold_part_1);
      r (part_2) := Do_as_JC_Moyer (20, threshold_part_2);
  end case;

  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 1360    (mini: 44  with threshold 1)
    --  Part 2: validated by AoC: 1005476 (mini: 285 with threshold 50)
  end if;
end AoC_2024_20;
