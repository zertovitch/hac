--  Solution to Advent of Code 2022, Day 17
-------------------------------------------
--  Pyroclastic Flow
--
--  A giant and deterministic Tetris without rotations.
--
--  https://adventofcode.com/2022/day/17
--  Copy of questions in: aoc_2022_17_questions.txt

with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

--  Interfaces is needed for compiling on both
--  HAC and GNAT (64-bit integer: Integer_64):
with Interfaces;

procedure AoC_2022_17 is
  use AoC_Toolbox, HAT, Interfaces;

  verbose : constant Natural := 0;

  subtype Range_x is Integer range 0 ..    8;
  subtype Range_y is Integer range 0 .. 8000;

  map   : array (Range_x, Range_y) of Character;

  lowest, highest : Point;

  procedure Adapt_Highest_Value_Point (using : Point) is
  begin
    highest.x := Max (highest.x, using.x);
    highest.y := Max (highest.y, using.y);
  end Adapt_Highest_Value_Point;

  procedure Show is
  begin
    for y in reverse lowest.y .. highest.y loop
      for x in lowest.x .. highest.x loop
        Put (map (x, y));
      end loop;
      New_Line;
    end loop;
  end Show;

  type Rock_Type is (minus, plus, L, I, square);

  --  ####
  --
  --  .#.
  --  ###
  --  .#.
  --
  --  ..#
  --  ..#
  --  ###
  --
  --  #
  --  #
  --  #
  --  #
  --
  --  ##
  --  ##

  rock : Rock_Type;

  --  pattern_mini : String (1 .. 40) := ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

  pattern : VString;

  jet : Positive;

  old_n_rock, old_height : Integer_64;
  rocks_per_cycle, height_per_cycle : Integer_64;
  target_rock : Integer_64 := 0;
  cycles : Integer_64;

  tera : constant := 1e12;

  procedure Simulate_Rock (n_rock : Integer_64) is
    lb   : Point;    --  Left, Bottom point of the "sprite".
    w, h : Integer;  --  Width, Height
    rt   : Point;    --  Right, Top
    fits, fit_px : Boolean;
    rest : Integer_64;
  begin
    --  Start position:
    lb.x := 3;
    lb.y := highest.y + 4;
    case rock is
      when minus  => w := 4; h := 1;
      when plus   => w := 3; h := 3;
      when L      => w := 3; h := 3;
      when I      => w := 1; h := 4;
      when square => w := 2; h := 2;
    end case;
    loop
      --  Gas jet:
      if Element (pattern, jet) = '<' then
        case rock is
          when plus =>
            fits := map (lb.x,     lb.y)     = '.' and then
                    map (lb.x - 1, lb.y + 1) = '.' and then
                    map (lb.x,     lb.y + 2) = '.';
          when L =>
            fits := map (lb.x - 1, lb.y)     = '.' and then
                    map (lb.x + 1, lb.y + 1) = '.' and then
                    map (lb.x + 1, lb.y + 2) = '.';
          when others =>
            fits := True;
            for y in 0 .. h - 1 loop
              fit_px := map (lb.x - 1, lb.y + y) = '.';
              fits := fits and then fit_px;
              exit when not fit_px;
            end loop;
        end case;
        if fits then
          lb.x := lb.x - 1;
        end if;
      else
        case rock is
          when plus =>
            fits := map (lb.x + 2, lb.y)     = '.' and then
                    map (lb.x + 3, lb.y + 1) = '.' and then
                    map (lb.x + 2, lb.y + 2) = '.';
          when others =>
            fits := True;
            for y in 0 .. h - 1 loop
              fit_px := map (lb.x + w, lb.y + y) = '.';
              fits := fits and then fit_px;
              exit when not fit_px;
            end loop;
        end case;
        if fits then
          lb.x := lb.x + 1;
        end if;
      end if;
      if jet = Length (pattern) then
        jet := 1;
        if old_n_rock /= 0 then
          --  We observe a cycle (the pattern having a prime
          --  number of symbols does the magic).
          --  Every rocks_per_cycle after n_rock - 1 + (any n >= 0),
          --  the tower is height_per_cycle higher.
          --
          --  We decompose tera = 1e12 falling rocks as:
          --
          --      tera = n_rock + rocks_per_cycle * cycles + rest
          --
          --  Hence, the relation
          --
          --     (tera - n_rock) = rocks_per_cycle * cycles + rest
          --
          --  allow us to calculate cycles and rest.
          --  Now we can see the decomposition like this:
          --
          --      tera = (n_rock + rest) + (rocks_per_cycle * cycles)
          --
          --  So, we can let falling (target_rock := n_rock + rest),
          --  measure the height h, let (rocks_per_cycle * cycles) more
          --  rocks falling. Then the total rocks is 1e12 and the total
          --  height is: h + height_per_cycle * cycles.
          --
          rocks_per_cycle  := n_rock - old_n_rock;
          height_per_cycle := Integer_64 (highest.y) - old_height;
          rest := (tera - n_rock) mod rocks_per_cycle;
          target_rock := n_rock + rest;
          cycles := (tera - n_rock) / rocks_per_cycle;
          if verbose > 0 then
            Put_Line
              (+"After rock nb " & Integer_64'Image (n_rock - 1) &
                " heap is height " & highest.y &
                " cycle rock " & Integer_64'Image (rocks_per_cycle) &
                "; cycle height: " & Integer_64'Image (height_per_cycle));
            Put_Line
              (+"Must reach rock nb " & Integer_64'Image (target_rock));
          end if;
        end if;
        old_n_rock := n_rock;
        old_height := Integer_64 (highest.y);
      else
        jet := jet + 1;
      end if;
      --  Simulate falling:
      case rock is
        when plus =>
          fits := map (lb.x,     lb.y)     = '.' and then
                  map (lb.x + 1, lb.y - 1) = '.' and then
                  map (lb.x + 2, lb.y)     = '.';
        when others =>
          fits := True;
          for x in 0 .. w - 1 loop
            fit_px := map (lb.x + x, lb.y - 1) = '.';
            fits := fits and then fit_px;
            exit when not fit_px;
          end loop;
      end case;
      if fits then
        lb.y := lb.y - 1;
      else
        --  "Write" the sprite
        case rock is
          when minus =>
            for x in 0 .. 3 loop
              map (lb.x + x, lb.y) := '#';
            end loop;
          when plus =>
            map (lb.x + 1, lb.y + 2) := '#';
            for x in 0 .. 2 loop
              map (lb.x + x, lb.y + 1) := '#';
            end loop;
            map (lb.x + 1, lb.y) := '#';
          when L =>
            map (lb.x + 2, lb.y + 2) := '#';
            map (lb.x + 2, lb.y + 1) := '#';
            for x in 0 .. 2 loop
              map (lb.x + x, lb.y) := '#';
            end loop;
          when I =>
            for y in 0 .. 3 loop
              map (lb.x, lb.y + y) := '#';
            end loop;
          when square =>
            for x in 0 .. 1 loop
              for y in 0 .. 1 loop
                map (lb.x + x, lb.y + y) := '#';
              end loop;
            end loop;
        end case;
        exit;
      end if;
    end loop;
    --  Calculate Right-Top corner.
    rt.x := lb.x + w - 1;
    rt.y := lb.y + h - 1;
    Adapt_Highest_Value_Point (rt);

    if rock = square then
      rock := minus;
    else
      rock := Rock_Type'Succ (rock);
    end if;
  end Simulate_Rock;

  f : File_Type;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer_64;

begin
  r (1) := 0;
  r (2) := 0;
  Open (f, "aoc_2022_17.txt");
  Get_Line (f, pattern);
  --  pattern := + pattern_mini;
  Close (f);
  lowest.x  := Range_x'First;
  highest.x := Range_x'Last;
  lowest.y  := Range_y'First;
  highest.y := 0;
  map (0, 0) := '+';
  map (0, 8) := '+';
  for x in 1 .. 7 loop
    map (x, 0) := '-';
  end loop;
  for y in 1 .. Range_y'Last loop
    map (0, y) := '|';
    for x in 1 .. 7 loop
      map (x, y) := '.';
    end loop;
    map (8, y) := '|';
  end loop;
  rock := minus;
  jet := 1;
  old_n_rock := 0;
  for rock_count in 1 .. 6000 loop
    Simulate_Rock (Integer_64 (rock_count));
    if verbose > 0 and then rock_count <= 5 then
      Put_Line (rock_count, 0);
      Show;
    end if;
    if rock_count = 2022 then
      r (1) := Integer_64 (highest.y);
    elsif Integer_64 (rock_count) = target_rock then
      r (2) := Integer_64 (highest.y) + cycles * height_per_cycle;
      exit;
    end if;
  end loop;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Height of the tower of...");
    Put_Line (+"  (part 1): 2022 rocks . . . . . . . . :" & Integer_64'Image (r (1)));
    Put_Line (+"  (part 2): 1_000_000_000_000 rocks  . :" & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 3193
    --  Part 2: validated by AoC: 1577650429835
  end if;
end AoC_2022_17;
