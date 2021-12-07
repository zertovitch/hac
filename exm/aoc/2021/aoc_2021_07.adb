--  Solution to Advent of Code 2021, Day 7
------------------------------------------
--  The Treachery of Whales
--
--  https://adventofcode.com/2021/day/7
--  Copy of questions in: aoc_2021_07_questions.txt
--
with HAL;  --  For a build with "full Ada": files hal*.ad* are in ../../../src

procedure AoC_2021_07 is
  use HAL;
  --
  input : constant VString := +"aoc_2021_07.txt";
  --
  sep : Character;
  f : File_Type;
  r : array (1 .. 2) of Integer;
  x_min_min : constant := 0;
  x_max_max : constant := 2000;
  subtype Position_Range is Integer range x_min_min .. x_max_max;
  type Crab_Population is array (Position_Range) of Natural;
  pop : Crab_Population;
  xx, x_min, x_max, total,
  cost_1, cost, cost_min,
  dist, x_cost_min,
  s_px, s_x_px, s_x2_px, total_x_px, total_x2_px : Integer;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := True;
  fast : constant Boolean := True;
begin
  for j in Position_Range loop
    pop (j) := 0;
  end loop;
  total := 0;
  Open (f, input);
  loop
    Get (f, xx);
    pop (xx) := pop (xx) + 1;
    total := total + 1;
    if total = 1 then
      x_min := xx;
      x_max := xx;
    end if;
    x_min := Min (x_min, xx);
    x_max := Max (x_max, xx);
    exit when End_Of_File (f);
    Get (f, sep);
  end loop;
  Close (f);
  if verbose then
    Put_Line (+"Pos min: " & x_min);
    Put_Line (+"Pos max: " & x_max);
    Put_Line (+"Total: " & total);
  end if;
  if fast then
    total_x_px := 0;
    total_x2_px := 0;
    for x in x_min .. x_max loop
      total_x_px  := total_x_px  + x * pop (x);
      total_x2_px := total_x2_px + x * x * pop (x);
    end loop;
  end if;
  --
  for part in 1 .. 2 loop
    cost_min := total * x_max_max ** 2;
    if fast then
      s_px    := 0;
      s_x_px  := 0;
      s_x2_px := 0;
    end if;
    for y in x_min .. x_max loop
      --  Compute the cost of moving all crabs to position y.
      if fast and part = 1 then
        s_px    := s_px   + pop (y);           --  Partial sum: sum_{x <= y} pop_x
        s_x_px  := s_x_px + y * pop (y);       --  Partial sum: sum_{x <= y} x * pop_x
        s_x2_px := s_x2_px + y * y * pop (y);  --  Partial sum: sum_{x <= y} x * x * pop_x
        --
        cost := y * (2 * s_px - total) - 2 * s_x_px + total_x_px;
        --
        --  To do !!
        --  Part 2's formula is in the works...
        --
      else
        cost := 0;
        for x in x_min .. x_max loop
          dist := abs (x - y);
          if part = 1 then
            cost_1 := dist;
          else
            cost_1 := dist * (dist + 1) / 2;
          end if;
          cost := cost + cost_1 * pop (x);
        end loop;
      end if;
      if cost < cost_min then
        cost_min := cost;
        x_cost_min := y;
      end if;
    end loop;
    if verbose then
       Put_Line (+"Minimal cost: " & cost_min & " at x = " & x_cost_min);
    end if;
    r (part) := cost_min;
  end loop;
  --
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: : " & r (1));
    Put_Line (+"Part 2: : " & r (2));
    --  Part 1: validated by AoC: 340052
    --  Part 2: validated by AoC: 92948968
  end if;
end AoC_2021_07;
