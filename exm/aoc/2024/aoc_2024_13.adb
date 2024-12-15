--  Solution to Advent of Code 2024, Day 13
-------------------------------------------
--  Claw Contraption
--
--  https://adventofcode.com/2024/day/13
--  Copy of questions in: aoc_2024_13_questions.txt
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

procedure AoC_2024_13 is

  use AoC_Toolbox, HAT, Interfaces;

  subtype I64 is Integer_64;

  --  The wording is misleading (on purpose of course!),
  --  with portions such as "...the cheapest way to win the prize..."
  --  or "...the minimum tokens you would have to spend..."
  --  Actually there is at most *one* solution in the real plane (\IR^2),
  --  thus also at most one solution in the square lattice (\ZZ^2).
  --  So, if there is a solution, we already have the lowest cost.
  --
  function Cost (ax, ay, bx, by, px, py : I64) return I64 is
    det : constant I64 := ax * by - bx * ay;
    a_times_det, b_times_det, a, b : I64;
  begin

    --  /    \       /        \     /   \
    --  | px |       | ax  bx |     | a |
    --  |    |   =   |        |  *  |   |
    --  | py |       | ay  by |     | b |
    --  \    /       \        /     \   /

    --  /   \       /        \-1   /    \
    --  | a |       | ax  bx |     | px |
    --  |   |   =   |        |  *  |    |
    --  | b |       | ay  by |     | py |
    --  \   /       \        /     \    /

    --             /        \-1     /          \
    --             | ax  bx |       | +by  -bx |
    --    where:   |        |   =   |          | / (ax by - bx ay)
    --             | ay  by |       | -ay  +ax |
    --             \        /       \          /

    if px = 0 and py = 0 then
      --  Case not seen on our input data.
      Put_Line ("px = 0, py = 0 ------> solution: a = 0, b = 0");
      return 0;
    end if;

    if det = 0 then
      --  Case not seen on our input data: a row is a multiple of the other.
      --     -> Zero or an infinity of solutions in \IR^2.
      --     -> Zero or a smaller infinity of solutions in \ZZ^2.
      --  We would have to look for the pair
      --  of natural integers with the smallest cost...
      Put_Line ("Determinant 0");
      return 0;
    end if;

    a_times_det := by * px - bx * py;
    b_times_det := ax * py - ay * px;

    if a_times_det rem det /= 0 or else b_times_det rem det /= 0 then
      --  There are remainders -> non integer solution
      --  (solution is in the plane \IR^2 but not in the square lattice \ZZ^2).
      return 0;
    end if;

    a := a_times_det / det;
    b := b_times_det / det;

    if a < 0 or b < 0 then
      --  Case not seen on our input data.
      Put_Line ("Negative number of button presses");
      return 0;
    end if;

    if a = 0 or b = 0 then
      --  Case not seen on our input data.
      Put_Line ("Solution with 1 or 2 buttons pressed 0 times");
    end if;

    return a * 3 + b;

  end Cost;

  r : array (Part_Type) of I64;

  procedure Read_Data_and_Solve is
    butax, butbx : String (1 .. 12) := "Button A: X+";
    butay, butby : String (1 .. 4)  := ", Y+";
    prix : String (1 .. 9)  := "Prize: X=";
    priy : String (1 .. 4)  := ", Y=";
    ax, ay, bx, by, px, py : Integer;
    f : File_Type;
    big : constant := 10_000_000_000_000;
  begin
    Open (f, "aoc_2024_13.txt");

    while not End_Of_File (f) loop
      Get (f, butax);  Get (f, ax);
      Get (f, butay);  Get (f, ay);
      Get (f, butbx);  Get (f, bx);
      Get (f, butby);  Get (f, by);
      Get (f, prix);   Get (f, px);
      Get (f, priy);   Get (f, py);

      r (part_1) :=
        r (part_1) +
        Cost (I64 (ax), I64 (ay), I64 (bx), I64 (by), I64 (px),       I64 (py));

      r (part_2) :=
        r (part_2) +
        Cost (I64 (ax), I64 (ay), I64 (bx), I64 (by), I64 (px) + big, I64 (py) + big);

    end loop;

    Close (f);
  end Read_Data_and_Solve;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data_and_Solve;

  if compiler_test_mode then
    if +r (part_1)'Image /= +' ' & Argument (1) or
       +r (part_2)'Image /= +' ' & Argument (2)
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & r (part_1)'Image);
    Put_Line (+"Part 2:" & r (part_2)'Image);
    --  Part 1: validated by AoC: 36954
    --  Part 2: validated by AoC: 79352015273424
  end if;
end AoC_2024_13;
