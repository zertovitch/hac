--  Solution to Advent of Code 2024, Day 22
-------------------------------------------
--  Monkey Market
--
--  https://adventofcode.com/2024/day/22
--  Copy of questions in: aoc_2024_22_questions.txt
--
--  HAC 0.40 "nice-to-have"'s detected in this exercise:
--
--    *     Modular types (esp. Unsigned_X and bitwise operators)
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

procedure AoC_2024_22 is

  use AoC_Toolbox, HAT, Interfaces;

  input_name : constant VString := +"aoc_2024_22_mini";
  --  input_name : constant VString := +"aoc_2024_22";

  r : array (Part_Type) of VString;

  subtype U64 is Integer_64;  --  Hack for HAC. "Full Ada": type U64 is mod 2 ** 64;

  total : U64 := 0;

  subtype Diff_Range is Integer range -9 .. +9;

  unseen : constant := -1;

  first_price        : array (Diff_Range, Diff_Range, Diff_Range, Diff_Range) of Integer;
  total_first_price  : array (Diff_Range, Diff_Range, Diff_Range, Diff_Range) of Natural;

  procedure Generate_2000 (initial_seed : U64) is
    x : U64 := initial_seed;
    x10, x10_old, d0, d1, d2, d3 : Integer := 0;
  begin
    --  "Full Ada" does it in a single instruction.
    for a in Diff_Range loop
      for b in Diff_Range loop
        for c in Diff_Range loop
          for d in Diff_Range loop
            first_price (a, b, c, d) := unseen;
          end loop;
        end loop;
      end loop;
    end loop;

    for count in 1 .. 2000 loop
      --  NB: a "Full Ada" compiler has the `xor` operator of course.
      x := Sim_XOR (x, x * 64)   mod 16777216;
      x := Sim_XOR (x, x / 32)   mod 16777216;
      x := Sim_XOR (x, x * 2048) mod 16777216;

      x10_old := x10;
      x10 := Integer (x mod 10);

      d3 := d2;
      d2 := d1;
      d1 := d0;
      d0 := x10 - x10_old;
      if count > 4 then
        if first_price (d3, d2, d1, d0) = unseen then
          first_price (d3, d2, d1, d0) := x10;
        end if;
      end if;
    end loop;
    total := total + x;

    for a in Diff_Range loop
      for b in Diff_Range loop
        for c in Diff_Range loop
          for d in Diff_Range loop
            if first_price (a, b, c, d) /= unseen then
              total_first_price (a, b, c, d) :=
                total_first_price (a, b, c, d) + first_price (a, b, c, d);
            end if;
          end loop;
        end loop;
      end loop;
    end loop;
  end Generate_2000;

  procedure Read_Data is
    i : Integer;
    f : File_Type;
  begin
    --  "Full Ada" does it in a single instruction.
    for a in Diff_Range loop
      for b in Diff_Range loop
        for c in Diff_Range loop
          for d in Diff_Range loop
            total_first_price (a, b, c, d) := 0;
          end loop;
        end loop;
      end loop;
    end loop;

    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get (f, i);
      Generate_2000 (U64 (i));
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
  begin
    r (part_1) := Trim_Left (+total'Image);
  end Do_Part_1;

  procedure Do_Part_2 is
    max_price : Integer := 0;
    p : Integer;
  begin
    --  "Full Ada 2012+" does it in a single loop (for ... of).
    for a in Diff_Range loop
      for b in Diff_Range loop
        for c in Diff_Range loop
          for d in Diff_Range loop
            p := total_first_price (a, b, c, d);
            if p > max_price then
              max_price := p;
            end if;
          end loop;
        end loop;
      end loop;
    end loop;
    r (part_2) := Trim_Left (+max_price'Image);
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 16619522798 (mini 1: 37327623, mini 2: 37990510)
    --  Part 2: validated by AoC: 1854 (mini 2: 23)
  end if;
end AoC_2024_22;
