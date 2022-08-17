--  Solution to Advent of Code 2021, Day 7
------------------------------------------
--  The Treachery of Whales
--
--  Interesting here: the `fast` mode is running in O(n)
--      where n is the span of x positions (x_max - x_min + 1).
--      The `slow` and straightforward way is running in O(n**2):
--      one outer loop over all positions, one inner loop for
--      computing the cost of moving all crabs to the position
--      considered in the outer loop.
--
--  https://adventofcode.com/2021/day/7
--  Copy of questions in: aoc_2021_07_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2021_07 is
  use HAT, Interfaces;
  --
  input : constant VString := +"aoc_2021_07.txt";
  --
  sep : Character;
  f : File_Type;
  r : array (1 .. 2) of Integer;
  x_min_min : constant := 0;
  x_max_max : constant := 2000;
  subtype Position_Range is Integer_64 range x_min_min .. x_max_max;
  type Crab_Population is array (Position_Range) of Integer_64;
  pop : Crab_Population;
  xx_inp : Integer;
  xx, x_min, x_max,
  cost_1, cost, cost_min,
  dist, x_cost_min,
  s_px, s_x_px, total, total_x_px, total_x2_px : Integer_64;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;
  fast : constant Boolean := True;
  T0 : constant Time := Clock;
begin
  for j in Position_Range loop
    pop (j) := 0;
  end loop;
  total := 0;
  Open (f, input);
  loop
    Get (f, xx_inp);
    xx := Integer_64 (xx_inp);
    pop (xx) := pop (xx) + 1;
    total := total + 1;
    if total = 1 then
      x_min := xx;
      x_max := xx;
    end if;
    if xx < x_min then x_min := xx; end if;
    if xx > x_max then x_max := xx; end if;
    exit when End_Of_File (f);
    Get (f, sep);
  end loop;
  Close (f);
  if verbose then
    Put_Line (+"Pos min:" & Integer_64'Image (x_min));
    Put_Line (+"Pos max:" & Integer_64'Image (x_max));
    Put_Line (+"Total:" & Integer_64'Image (total));
  end if;
  if fast then
    total_x_px := 0;
    total_x2_px := 0;
    for x in x_min .. x_max loop
      total_x_px  := total_x_px  + Integer_64 (x) * pop (x);
      total_x2_px := total_x2_px + Integer_64 (x * x) * pop (x);
    end loop;
  end if;
  --
  cost_min := 0;  --  Just calm down a warning.
  for part in 1 .. 2 loop
    if fast then
      s_px   := 0;
      s_x_px := 0;
    end if;
    for y in x_min .. x_max loop
      --  Compute the cost of moving all crabs to position y.
      if fast then
        --  Quick computation without inner loop:
        s_px   := s_px   + pop (y);                   --  Partial sum: sum_{x <= y} pop_x
        s_x_px := s_x_px + Integer_64 (y) * pop (y);  --  Partial sum: sum_{x <= y} x * pop_x
        --
        if part = 1 then
          cost := y * (2 * s_px - total) - 2 * s_x_px + total_x_px;
        else
          cost :=      (2 * y *        s_px -           --
                        2 *            s_x_px +         --   Maths,
                                       total_x2_px +    --   pencil & paper
                        (1 - 2 * y)  * total_x_px +     --
                        (y ** 2 - y) * total)           --   --> 639x faster!
                   / 2;                                 --
        end if;
      else
        --  Slow, straightforward computation, with inner loop:
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
      --  Did we find an optimum ?
      if y = x_min or cost < cost_min then
        cost_min := cost;
        x_cost_min := y;
      end if;
    end loop;
    if verbose then
       Put_Line
         ("Minimal cost:" & Integer_64'Image (cost_min) &
          " at x =" & Integer_64'Image (x_cost_min));
    end if;
    r (part) := Integer (cost_min);
  end loop;
  --
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: sum of linear costs: " & r (1));
    Put_Line (+"Part 2: sum of quadratic costs: " & r (2));
    --  Part 1: validated by AoC: 340052
    --  Part 2: validated by AoC: 92948968
  end if;
end AoC_2021_07;
