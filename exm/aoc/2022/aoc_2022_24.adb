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

  procedure GCD_and_Bezout (a, b : in Integer; s, t, the_gcd : out Integer) is
    --  Finds the GCD and s, t for the
    --  ` GCD (a, b) = a * s + b * t ` factorization (Bezout theorem).
    --  Program 1.8, Introduction to number theory, RBJT Allenby & EJ Redfern
    ta, tb : array (1 .. 3) of Integer;
    q, r : Integer;
  begin
    ta (1) := 1;         tb (1) := 0;
    ta (2) := 0;         tb (2) := 1;
    ta (3) := a;         tb (3) := b;
    while tb (3) /= 0 loop
      q := ta (3) / tb (3);
      for i in 1 .. 3 loop
        r := ta (i) - q * tb (i);
        ta (i) := tb (i);
        tb (i) := r;
      end loop;
    end loop;
    s :=       ta (1);
    t :=       ta (2);
    the_gcd := ta (3);
  end GCD_and_Bezout;

  function GCD (a, b : Integer) return Integer is
    s, t, the_gcd : Integer;
  begin
    GCD_and_Bezout (a, b, s, t, the_gcd);
    return the_gcd;
  end GCD;

  --  Convention: 0-based, y axis appears top -> down.

  type Blizzard is array (Range_x, Range_y) of Boolean;

  lb, ub, rb, db : Blizzard;
  best : array (Range_x, Range_y, Range_t) of Natural;

  procedure Show (time_step : Natural) is
  begin
    for y in lowest.y .. highest.y loop  --  y axis appears top -> down.
      for x in lowest.x .. highest.x loop
        if    lb ((x + time_step) mod (highest.x + 1), y) then Put ('<');
        elsif ub (x, (y + time_step) mod (highest.y + 1)) then Put ('^');
        elsif rb ((x - time_step) mod (highest.x + 1), y) then Put ('>');
        elsif db (x, (y - time_step) mod (highest.y + 1)) then Put ('v');
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
    len  : Natural;  --  Length in space-time
    pt   : Point;    --  dimensions: x, y
    t    : Natural;  --  dimension:  t
    pred : Natural;  --  This is just for displaying the path.
  end record;

  function Node_Image (n : Node) return VString is
  begin
    return
      +" x = " & n.pt.x & ", y = " & n.pt.y & ", t = " & n.t;
  end Node_Image;

  list : array (List_Range) of Node;
  current, explored : Natural;  --  0 <= current <= explored

  function Dijkstra_Algorithm (start : Point; time_start : Natural; part : Part_Type) return Natural is

    blizzard_cycle : Positive;
    cur_pt  : Point;
    cur_len : Natural;

    time_step : Natural := time_start;

    procedure Visit (x, y : Integer) is
      len_to, ins, t : Integer;
      ok_step : Boolean;
    begin
      t := time_step + 1;
      if (x in 0 .. highest.x and then y in 0 .. highest.y)
        or else (x = 0 and then y = -1)                     --  The starting block.
        or else (x = highest.x and then y = highest.y + 1)  --  The end (and new start...).
      then
        ok_step := y = -1
           or else y = highest.y + 1
           or else
             not         (lb ((x + t) mod (highest.x + 1), y)
                  or else rb ((x - t) mod (highest.x + 1), y)
                  or else ub (x, (y + t) mod (highest.y + 1))
                  or else db (x, (y - t) mod (highest.y + 1)));
        if ok_step then
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
            list (ins).t    := t;
            list (ins).pred := current;
            explored := explored + 1;
            if verbosity_level > 1 then
              Put_Line (" ->   added");
            end if;
          else
            if verbosity_level > 1 then
              Put_Line (" ->   NOT added");
            end if;
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

    best (start.x, start.y, time_step mod blizzard_cycle) := 0;
    cur_pt := start;
    loop
      cur_len := best (cur_pt.x, cur_pt.y, time_step mod blizzard_cycle);
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
      time_step := list (current).t;
      if verbosity_level > 0 then
        Put_Line
          (+" === > Current: [" & Node_Image (list (current)) &
           "], current = " & current);
      end if;
      case part is
        when part_1 =>
          exit when cur_pt.x = highest.x and then cur_pt.y = highest.y;
        when part_2 =>
          exit when cur_pt.x = 0 and then cur_pt.y = 0;
      end case;
    end loop;
    return time_step + 1;
  end Dijkstra_Algorithm;

  T0 : constant Time := Clock;
  r : array (Part_Type) of Integer;

  start : Point;

begin
    Clear (lb);
    Clear (ub);
    Clear (rb);
    Clear (db);
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
          when '<' => lb (i - 2, highest.y) := True;
          when '^' => ub (i - 2, highest.y) := True;
          when '>' => rb (i - 2, highest.y) := True;
          when 'v' => db (i - 2, highest.y) := True;
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
    r (part_1) := Dijkstra_Algorithm (start, 0, part_1);
    start.x := highest.x;
    start.y := highest.y + 1;
    r (part_2) := Dijkstra_Algorithm (start, r (part_1), part_2);
    start.x := 0;
    start.y := -1;
    r (part_2) := Dijkstra_Algorithm (start, r (part_2), part_1);

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (part_1) /= Integer'Value (To_String (Argument (1))) or
       r (part_2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Shortest time for...");
    Put_Line (+"  (part 1) one trip . . . . :" & r (part_1)'Image);
    Put_Line (+"  (part 2) three trips  . . :" & r (part_2)'Image);
    --  Part 1: validated by AoC: 238
    --  Part 2: validated by AoC: 751
  end if;
end AoC_2022_24;
