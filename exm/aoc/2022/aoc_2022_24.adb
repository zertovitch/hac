--  Solution to Advent of Code 2022, Day 24
-------------------------------------------
--  Blizzard Basin
--
--  https://adventofcode.com/2022/day/24
--  Copy of questions in: aoc_2022_24_questions.txt

--  Runs in 0.12 second with GNAT.
--  Runs in 134 seconds with HAC.

with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

procedure AoC_2022_24 is
  use AoC_Toolbox, HAT;

  verbosity_level : constant Natural := 0;

  f : File_Type;
  s : VString;

  subtype Range_x is Integer range  0 .. 101;
  subtype Range_y is Integer range -1 ..  36;
  subtype Range_t is Integer range  0 .. 700;

  lowest, highest : Point;

  inf : constant Natural := Integer'Last / 4;

  --  Convention: 0-based, y axis appears top -> down.

  type Blizzard is array (Range_x, Range_y) of Boolean;

  leftward_blizzard, upward_blizzard,
  rightward_blizzard, downward_blizzard : Blizzard;

  best : array (Range_x, Range_y, Range_t) of Natural;

  procedure Show (time_step : Natural) is
  begin
    for y in lowest.y .. highest.y loop  --  y axis appears top -> down.
      for x in lowest.x .. highest.x loop
        if    leftward_blizzard  ((x + time_step) mod (highest.x + 1), y) then Put ('<');
        elsif upward_blizzard    (x, (y + time_step) mod (highest.y + 1)) then Put ('^');
        elsif rightward_blizzard ((x - time_step) mod (highest.x + 1), y) then Put ('>');
        elsif downward_blizzard  (x, (y - time_step) mod (highest.y + 1)) then Put ('v');
        else Put ('.');
        end if;
      end loop;
      New_Line;
    end loop;
  end Show;

  procedure Clear (b : in out Blizzard) is
  begin
    for y in Range_y loop
      for x in Range_x loop
        b (x, y) := False;
      end loop;
    end loop;
  end Clear;

  --  Dijkstra shortest path algorithm.
  --  Code adapted from AoC_2022_12.
  --

  list_length_max : constant := 200_000;  --  Adjusted by doubling until no Constraint_Error.

  subtype List_Range is Integer range 1 .. list_length_max;

  type Node is record
    len  : Natural;   --  Length in space-time
    pt   : Point_3D;  --  dimensions: x, y, t
    pred : Natural;   --  This is just for displaying the path.
  end record;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  stats : constant Boolean := True;
  stats_first_visit      : Natural := 0;
  stats_added_revisit    : Natural := 0;
  stats_rejected_revisit : Natural := 0;

  function Dijkstra_Algorithm (start : Point_3D; part : Part_Type) return Natural is

    blizzard_cycle : Positive;
    cur_pt  : Point_3D;
    cur_len : Natural;

    procedure Visit (x, y : Integer) is
      len_to, ins, t : Integer;
    begin
      t := cur_pt.z + 1;
      if         (x in 0 .. highest.x    --  The valley.
         and then y in 0 .. highest.y
         and then not
                  (leftward_blizzard  ((x + t) mod (highest.x + 1), y)
           or else rightward_blizzard ((x - t) mod (highest.x + 1), y)
           or else upward_blizzard    (x, (y + t) mod (highest.y + 1))
           or else downward_blizzard  (x, (y - t) mod (highest.y + 1))))

        or else (x = 0 and then y = -1)  --  The "starting block".
        or else
                  (x = highest.x         --  The end (and new start...).
          and then y = highest.y + 1)
      then
        len_to := cur_len + 1;
        if verbosity_level > 1 then
          Put (+"x: " & x & ", y: " & y &
               ", t: " & t & "  cur_len = " & cur_len);
          if best (x, y, t mod blizzard_cycle) < inf then
            Put
              (+"  old best (x, y, time_step) = " &
               best (x, y, t mod blizzard_cycle) & ", ");
          else
            Put ("  not visited, ");
          end if;
        end if;
        if len_to < best (x, y, t mod blizzard_cycle) then
          --  Improvement on cell (x, y).
          best (x, y, t mod blizzard_cycle) := len_to;
          --
          --  Insert in a sorted way.
          --
          ins := explored + 1;
          for i in current + 1 .. explored loop
            if current < list (i).len then
              ins := i;  --  Insert here.
              --  Optional: remove another node
              --  with the same (x, y) and a larger length.
              exit;
            end if;
          end loop;
          for i in reverse ins .. explored loop
            list (i + 1) := list (i);
          end loop;
          list (ins).len := len_to;
          list (ins).pt.x := x;
          list (ins).pt.y := y;
          list (ins).pt.z := t;
          list (ins).pred := current;
          explored := explored + 1;
          if verbosity_level > 1 then
            Put_Line (" ->   added");
          end if;
          if stats then
            if best (x, y, t mod blizzard_cycle) < inf then
              stats_first_visit := stats_first_visit + 1;
            else
              stats_added_revisit := stats_added_revisit + 1;
            end if;
          end if;
        else
          if stats then
            if best (x, y, t mod blizzard_cycle) < inf then
              stats_rejected_revisit := stats_rejected_revisit + 1;
            else
              put ("Uh (rejected but = inf) ?");
            end if;
          end if;
          if verbosity_level > 1 then
            Put_Line (" ->   NOT added");
          end if;
        end if;
      end if;
    end Visit;

  begin
    current  := 0;
    explored := 0;
    blizzard_cycle :=
      (highest.x + 1) * (highest.y + 1) / GCD (highest.x + 1, highest.y + 1);
    if verbosity_level > 0 then
      Put_Line ("blizzard_cycle" & blizzard_cycle'Image);
    end if;

    for t in 0 .. Min (blizzard_cycle - 1, best'Last (3)) loop
      for x in 0 .. highest.x loop
        for y in -1 .. highest.y + 1 loop
          best (x, y, t) := inf;
        end loop;
      end loop;
    end loop;

    cur_pt := start;
    best (cur_pt.x, cur_pt.y, cur_pt.z mod blizzard_cycle) := 0;
    loop
      cur_len := best (cur_pt.x, cur_pt.y, cur_pt.z mod blizzard_cycle);
      Visit (cur_pt.x,     cur_pt.y);  --  Wait in place.
      Visit (cur_pt.x - 1, cur_pt.y);
      Visit (cur_pt.x + 1, cur_pt.y);
      Visit (cur_pt.x, cur_pt.y - 1);
      Visit (cur_pt.x, cur_pt.y + 1);
      --
      --  Switch to the next best explored point.
      --
      current := current + 1;
      if current > explored then
        Put_Line ("No way found.");
        return inf;
      end if;
      cur_pt := list (current).pt;
      case part is
        when part_1 =>
          exit when cur_pt.x = highest.x
           and then cur_pt.y = highest.y;
        when part_2 =>
          exit when cur_pt.x = 0
           and then cur_pt.y = 0;
      end case;
    end loop;
    return cur_pt.z + 1;
  end Dijkstra_Algorithm;

  T0 : constant Time := Clock;
  r : array (Part_Type) of Integer;

  start : Point_3D;

begin
    Clear (leftward_blizzard);
    Clear (upward_blizzard);
    Clear (rightward_blizzard);
    Clear (downward_blizzard);
    lowest.x := 0;
    lowest.y := 0;
    Open (f, "aoc_2022_24.txt");
    Get_Line (f, s);
    highest.x := Length (s) - 3;
    highest.y := -1;
  Read_Data :
    while not End_Of_File (f) loop
      Get_Line (f, s);
      exit when End_Of_File (f);  --  Discard last line
      highest.y := highest.y + 1;
      for i in 2 .. Length (s) - 1 loop
        case Element (s, i) is
          when '.' => null;
          when '<' => leftward_blizzard (i - 2, highest.y) := True;
          when '^' => upward_blizzard (i - 2, highest.y) := True;
          when '>' => rightward_blizzard (i - 2, highest.y) := True;
          when 'v' => downward_blizzard (i - 2, highest.y) := True;
          when others => Put ("Whut?");
        end case;
      end loop;
    end loop Read_Data;
    Close (f);

    if verbosity_level > 0 then
      for i in 0 .. 18 loop
        Put_Line (i, 0);
        Show (i);
        New_Line;
      end loop;
    end if;

    start.x := 0;
    start.y := -1;
    start.z := 0;
    r (part_1) := Dijkstra_Algorithm (start, part_1);
    start.x := highest.x;
    start.y := highest.y + 1;
    start.z := r (part_1);
    r (part_2) := Dijkstra_Algorithm (start, part_2);
    start.x := 0;
    start.y := -1;
    start.z := r (part_2);
    r (part_2) := Dijkstra_Algorithm (start, part_1);

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (part_1) /= Integer'Value (To_String (Argument (1))) or
       r (part_2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    if stats then
      Put_Line (+"Firstly visited cells    : " & stats_first_visit);
      Put_Line (+"Added revisited cells    : " & stats_added_revisit);
      Put_Line (+"Rejected revisited cells : " & stats_rejected_revisit);
    end if;
    Put_Line (+"Shortest time for...");
    Put_Line (+"  (part 1) one trip . . . . :" & r (part_1)'Image);
    Put_Line (+"  (part 2) three trips  . . :" & r (part_2)'Image);
    --  Part 1: validated by AoC: 238
    --  Part 2: validated by AoC: 751
  end if;
end AoC_2022_24;
