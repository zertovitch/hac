--  Solution to Advent of Code 2022, Day 23
-------------------------------------------
--  Unstable Diffusion
--
--  https://adventofcode.com/2022/day/23
--  Copy of questions in: aoc_2022_23_questions.txt

with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

procedure AoC_2022_23 is
  use AoC_Toolbox, HAT;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbosity_level : constant Natural := 0;

  radius : constant := 200;

  subtype Range_x is Integer range -radius .. radius;
  subtype Range_y is Integer range -radius .. radius;

  lowest, highest : Point;

  procedure Adapt_Lowest_Value_Point (using : Point) is
  begin
    lowest.x := Min (lowest.x, using.x);
    lowest.y := Min (lowest.y, using.y);
  end Adapt_Lowest_Value_Point;

  procedure Adapt_Highest_Value_Point (using : Point) is
  begin
    highest.x := Max (highest.x, using.x);
    highest.y := Max (highest.y, using.y);
  end Adapt_Highest_Value_Point;

  map : array (Range_x, Range_y) of Character;

  procedure Extend_Boundaries (p : Point) is
  begin
    Adapt_Lowest_Value_Point (p);
    Adapt_Highest_Value_Point (p);
  end Extend_Boundaries;

  elf             : constant Character := '#';
  elf_before_move : constant Character := 'x';
  elf_after_move  : constant Character := '@';

  procedure Clear_Map is
  begin
    for y in Range_y loop
      for x in Range_x loop
        map (x, y) := '.';
      end loop;
    end loop;
  end Clear_Map;

  procedure Clean_Map is
    c : Character;
  begin
    for y in lowest.y .. highest.y loop
      for x in lowest.x .. highest.x loop
        c := map (x, y);
        if c in '1' .. '4'
          or else c = elf_before_move
        then
          map (x, y) := '.';
        elsif c = elf_after_move then
          map (x, y) := elf;
        end if;
      end loop;
    end loop;
  end Clean_Map;

  elves : Natural;

  function Count_Empty_Ground return Natural is
    elf_low, elf_high : Point;
  begin
    elf_low.x := radius;
    elf_low.y := radius;
    elf_high.x := -radius;
    elf_high.y := -radius;
    for y in lowest.y .. highest.y loop
      for x in lowest.x .. highest.x loop
        if map (x, y) = elf then
          elf_low.x  := Min (elf_low.x, x);
          elf_low.y  := Min (elf_low.y, y);
          elf_high.x := Max (elf_high.x, x);
          elf_high.y := Max (elf_high.y, y);
        end if;
      end loop;
    end loop;
    return
      (elf_high.y - elf_low.y + 1) *
      (elf_high.x - elf_low.x + 1) - elves;
  end Count_Empty_Ground;

  type Direction is (N, S, W, E);

  function Next (d : Direction) return Direction is
  begin
    if d = Direction'Last then
      return Direction'First;
    else
      return Direction'Succ (d);
    end if;
  end Next;

  type Direction_Set is array (Direction) of Boolean;

  procedure Show is
  begin
    for y in reverse lowest.y .. highest.y loop  --  y axis appears bottom -> up.
      for x in lowest.x .. highest.x loop
        Put (map (x, y));
      end loop;
      New_Line;
    end loop;
  end Show;

  function Is_Elf (c : Character) return Boolean is
  begin
    return c = elf or else c = elf_before_move;
  end Is_Elf;

  start : Direction;
  any_move : Boolean;

  procedure Do_Round (verbosity_enabled : Boolean) is

    procedure Do_Half_Round (half : Positive) is
      elf_look : Direction;
      is_possible, all_possible : Direction_Set;
      dest : Point;
    begin
      for d in Direction loop
        all_possible (d) := True;
      end loop;
      for y in lowest.y .. highest.y loop
        for x in lowest.x .. highest.x loop
          if Is_Elf (map (x, y)) then
            is_possible := all_possible;
            if Is_Elf (map (x - 1, y + 1)) then
              is_possible (N) := False;
              is_possible (W) := False;
            end if;
            if Is_Elf (map (x + 1, y + 1)) then
              is_possible (N) := False;
              is_possible (E) := False;
            end if;
            if Is_Elf (map (x - 1, y - 1)) then
              is_possible (S) := False;
              is_possible (W) := False;
            end if;
            if Is_Elf (map (x + 1, y - 1)) then
              is_possible (S) := False;
              is_possible (E) := False;
            end if;
            if Is_Elf (map (x, y + 1)) then
              is_possible (N) := False;
            end if;
            if Is_Elf (map (x, y - 1)) then
              is_possible (S) := False;
            end if;
            if Is_Elf (map (x - 1, y)) then
              is_possible (W) := False;
            end if;
            if Is_Elf (map (x + 1, y)) then
              is_possible (E) := False;
            end if;
            if is_possible (N)
              and then is_possible (S)
              and then is_possible (W)
              and then is_possible (E)
            then
              --  This Elf has enough room around him,
              --  thus no incentive to move.
              null;
            else
              elf_look := start;
            Look_Directions :
              for dir_count in 1 .. 4 loop
                if is_possible (elf_look) then
                  --  Mark the destination cell.
                  case elf_look is
                    when N => dest.x := x; dest.y := y + 1;
                    when S => dest.x := x; dest.y := y - 1;
                    when W => dest.x := x - 1; dest.y := y;
                    when E => dest.x := x + 1; dest.y := y;
                  end case;
                  Extend_Boundaries (dest);
                  if half = 1 then
                    --  Consider a move in chosen direction:
                    case map (dest.x, dest.y) is
                      when '.' => map (dest.x, dest.y) := '1';
                      when '1' => map (dest.x, dest.y) := '2';
                      when '2' => map (dest.x, dest.y) := '3';
                      when '3' => map (dest.x, dest.y) := '4';
                      when others =>
                        Put ("Bug : " & map (dest.x, dest.y));
                    end case;
                  else
                    --  Move... but only if there is no other
                    --          Elf planning to move there.
                    if map (dest.x, dest.y) = '1' then
                      any_move := True;
                      map (x, y)           := elf_before_move;
                      map (dest.x, dest.y) := elf_after_move;
                      --  We don't put immediately `elf` on
                      --  destination cell.
                      --  The reason is to avoid double-moves
                      --  on adjacent cells (the Elves move all
                      --  at the same time)...
                    end if;
                  end if;
                  exit;
                end if;
                elf_look := Next (elf_look);
              end loop Look_Directions;
            end if;
          end if;
        end loop;
      end loop;
    end Do_Half_Round;

  begin
    Clean_Map;
    any_move := False;

    if verbosity_enabled and then verbosity_level > 1 then
      Put_Line ("--- after cleanup");
      Show;
    end if;

    for half in 1 .. 2 loop
      Do_Half_Round (half);
      if verbosity_enabled and then verbosity_level > 1 then
        Put_Line ("--- after half round" & half'Image);
        Show;
      end if;
    end loop;

    start := Next (start);
  end Do_Round;

  procedure Data_Acquisition is
    p : Point;
    f : File_Type;
    s : VString;
    c : Character;
  begin
    Clear_Map;
    lowest.x := radius;
    lowest.y := radius;
    highest.x := -radius;
    highest.y := -radius;
    Open (f, "aoc_2022_23.txt");
    p.y := 0;
    elves := 0;
    start := N;
  Read_Data :
    while not End_Of_File (f) loop
      Get_Line (f, s);
      p.y := p.y - 1;
      for i in 1 .. Length (s) loop
        p.x := i;
        c := Element (s, i);
        map (p.x, p.y) := c;
        if c = elf then
          elves := elves + 1;
          Adapt_Lowest_Value_Point (p);
          Adapt_Highest_Value_Point (p);
        end if;
      end loop;
    end loop Read_Data;
    Close (f);
  end Data_Acquisition;

  T0 : constant Time := Clock;
  r : array (Part_Type) of Integer;
  last_round : Integer := 10;

begin
  r (part_1) := 0;
  r (part_2) := 0;
Parts :
  for part in part_1 .. part_1 loop
    Data_Acquisition;
    if verbosity_level > 0 then
      Show;
    end if;
    for round in 1 .. 10 loop
      if verbosity_level > 0 then
        Put_Line ("===== Round" & round'Image & " =====");
      end if;
      Do_Round (True);
      if verbosity_level = 1 then
        Show;
      end if;
    end loop;
    Clean_Map;
    if verbosity_level > 0 then
      Put_Line ("===== 10th Round Evaluation =====");
      Show;
    end if;
    r (part_1) := Count_Empty_Ground;
    if not compiler_test_mode then
      for round in 11 .. Integer'Last loop
        exit when not any_move;
        last_round := round;
        Do_Round (False);
      end loop;
      if verbosity_level > 0 then
        Put_Line
          ("===== Stable state after" & last_round'Image &
           " rounds =====");
        Show; 
      end if;
      r (part_2) := last_round;
    end if;
  end loop Parts;

  if compiler_test_mode then
    if r (part_1) /= Integer'Value (To_String (Argument (1))) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"===== Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & r (part_1)'Image);
    Put_Line (+"Part 2:" & r (part_2)'Image);
    --  Part 1: validated by AoC: 3689
    --  Part 2: validated by AoC: 965
  end if;
end AoC_2022_23;
