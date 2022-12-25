--  Solution to Advent of Code 2022, Day 21
-------------------------------------------
--  Monkey Math
--
--  https://adventofcode.com/2022/day/21
--  Copy of questions in: aoc_2022_21_questions.txt

with AoC_2022_21_Pkg;
with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

--  Interfaces is needed for compiling on both
--  HAC and GNAT (64-bit integer: Integer_64):
with Interfaces;

procedure AoC_2022_21 is

  use AoC_2022_21_Pkg, AoC_Toolbox;
  use HAT, Interfaces;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := not compiler_test_mode;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer_64;

  function Search (choice : Data_Type) return Integer_64 is

    function f (x : Integer_64) return Integer_64 is
    begin
      case choice is
        when mini  => return Compute_Mini (part_2, x);
        when input => return Compute (part_2, x);
      end case;
    end f;

    x1, x2, x, y1, y2, y : Integer_64;

    procedure Show is
    begin
      Put_Line
        (+"   x in [" &
         Integer_64'Image (x1) & " .." &
         Integer_64'Image (x2) & "] : " &
         Integer_64'Image (y1) & ", " &
         Integer_64'Image (y2));
    end Show;

  begin
    x := 1;
    if verbose then
      Put_Line ("Range extension phase:");
    end if;
    loop
      x1 := -x;
      x2 := +x;
      y1 := f (x1);
      y2 := f (x2);
      if verbose then
        Show;
      end if;
      exit when Sgn_64 (y1) /= Sgn_64 (y2);
      x := x * 2;
    end loop;
    if y1 = 0 then
      return x1;
    elsif y2 = 0 then
      return x2;
    end if;
    --  From here, y1 and y2 are non-zero, with different signs.
    if verbose then
      Put_Line ("Range narrowing phase:");
    end if;
    loop
      x := (x1 + x2) / 2;
      y := f (x);
      if y = 0 then
        return x;
      end if;
      if Sgn_64 (y) = Sgn_64 (y1) then
        --  The zero is *not* between x1 and x.
        x1 := x;
        y1 := y;
      else
        x2 := x;
        y2 := y;
      end if;
      if verbose then
        Show;
      end if;
      exit when x1 = x2;
    end loop;
    Put_Line ("Non-integer zero...");
    return 666;
  end Search;

begin
  if verbose then
    Put_Line
      ("Example: simple computation: " &
       Integer_64'Image (Compute_Mini (part_1, 0)));
  end if;
  r (1) := Compute (part_1, 0);

  if verbose then
    for i in 300 .. 303 loop
      Put_Line
        (+"Example: difference at root, human = " & i & " : " &
         Integer_64'Image (Compute_Mini (part_2, Integer_64 (i))));
    end loop;
    Put_Line
      (+"Example: search minimum: " & Integer_64'Image (Search (mini)));
  end if;
  r (2) := Search (input);

  if verbose then
    Put_Line ("Some values around the zero:");
    for i in r (2) - 3 .. r (2) + 2 loop
      Put_Line
        ("   x = " &
         Integer_64'Image (i) & " : " &
         Integer_64'Image (Compute (part_2, i)));
    end loop;
    New_Line;
  end if;

  if compiler_test_mode then
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: evaluation . . . . :" & Integer_64'Image (r (1)));
    Put_Line (+"Part 2: search of zero . . :" & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 286698846151845
    --  Part 2: validated by AoC: 3759566892641
  end if;
end AoC_2022_21;
